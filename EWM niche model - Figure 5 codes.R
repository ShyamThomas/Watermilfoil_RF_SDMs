
library(ggplot2)
library(ggpubr)
library(tidyverse)

#### Figure 5. Suitability abundance relationships for all occurrence datasets: a-d

prsabs.sdm=read.sdm("MinnEWMresults/prsabs.SDM.sdm")
randabs.sdm=read.sdm("MinnEWMresults/randpseuabs.SDM.sdm")
outrandabs.autologistic.sdm=read.sdm("MinnEWMresults/outrandpseuabs.SDM.sdm")
nearrandabs.sdm=read.sdm("MinnEWMresults/nearrandpseuabs.SDM.sdm")

EWM.infes_relfrq.selpreds.prsabs=read_csv("MinnEWMdata/EWM.infes_relfrq.selpreds.prsabs.csv")
EWM.infes_relfrq.selpreds.prsabs
### Lets include connectivity variables: stream and road density
LakeIndex.ConnData = read_csv("MinnEWMdata/LakeConnectivityData.Reduced.csv")
colnames(LakeIndex.ConnData)
LakeConn=LakeIndex.ConnData%>%
  dplyr::select(c(DOWLKNUM, allstreams_density_mperha, roaddensity_density_mperha))
LakeConn
EWM.infes_relfrq.Conn.preds.prsabs=left_join(EWM.infes_relfrq.selpreds.prsabs,LakeConn, by="DOWLKNUM")
EWM.infes_relfrq.Conn.preds.prsabs

sdm.data=EWM.infes_relfrq.Conn.preds.prsabs[,-c(1:3,6)]%>%
  na.omit
sdm.data
sdm.data.df=as.data.frame(sdm.data)
dim(sdm.data.df)
head(sdm.data.df)

sdm.data.pred.df=sdm.data.df[,-c(1:3)]
head(sdm.data.pred.df)
predict.sdm.rf=predict(prsabs.sdm, newdata=sdm.data.pred.df, method="rf", mean=TRUE) ### predicted values from the presence-absence SDM above
head(predict.sdm.rf)
predict.sdm.rf.df=as.data.frame(predict.sdm.rf)

sdm.data.DOW.latlon=EWM.infes_relfrq.Conn.preds.prsabs.naomit[,c(1,4,5,7)]
sdm.data.DOW.latlon
sdm.data.DOW.latlon$predvalues=predict.sdm.rf.df$`sp_1-m_rf-re_subs`

predict.randpseuabs.sdm.rf=predict(randabs.sdm, newdata=sdm.data.pred.df, method="rf", mean=TRUE)
tail(predict.randpseuabs.sdm.rf)
predict.randpseuabs.sdm.rf.df=as.data.frame(predict.randpseuabs.sdm.rf)
sdm.data.DOW.latlon$randpeseuabs_predvalues=predict.randpseuabs.sdm.rf.df$`sp_1-m_rf-re_subs`
sdm.data.DOW.latlon

predict.outrand.pseuabs.sdm.rf=predict(outrandabs.sdm, newdata=sdm.data.pred.df, method="rf", mean=TRUE)
tail(predict.outrand.pseuabs.sdm.rf)
predict.outrand.pseuabs.sdm.rf.df=as.data.frame(predict.outrand.pseuabs.sdm.rf)
sdm.data.DOW.latlon$outrandpeseuabs_predvalues=predict.outrand.pseuabs.sdm.rf.df$`sp_1-m_rf-re_subs`
sdm.data.DOW.latlon
nearrandabs.sdm

predict.nearrand.pseuabs.sdm.rf=predict(nearrandabs.sdm, newdata=sdm.data.pred.df, method="rf", mean=TRUE)
tail(predict.nearrand.pseuabs.sdm.rf)
predict.nearrand.pseuabs.sdm.rf.df=as.data.frame(predict.nearrand.pseuabs.sdm.rf)
sdm.data.DOW.latlon$nearrandpeseuabs_predvalues=predict.nearrand.pseuabs.sdm.rf.df$`sp_1-m_rf-re_subs`
sdm.data.DOW.latlon

#### Now bring in occurrence frequency (abundance) data
EWM.infes_relfrq.preds=read_csv("MinnEWMdata/EWMinfes_relfrq.preds.data.csv")
EWM.infes_relfrq.preds
EWM.relfrq.preds.data=EWM.infes_relfrq.preds%>%
  filter(MYS_relfrq>=0)
EWM.relfrq.preds.data
Selvars.relfrq=c('DOWLKNUM', 'UTMX','UTMY','LON','LAT', 'MYS_relfrq' ,'avg_totalalkalinity','max_depth','size_acres', 
                 'avg_ph','avg_secchi','avg_conductance','avg_nitrogen','avg_phosphorus', 'avg_chlorophylla', 
                 'a440.cdom', 'mean.gdd_wtr_10c')
EWM.relfrq.preds.data=EWM.relfrq.preds.data[Selvars.relfrq]
EWM.relfrq.Conn.preds.data=left_join(EWM.relfrq.preds.data,LakeConn, by="DOWLKNUM")
EWM.relfrq.index=EWM.relfrq.Conn.preds.data[,c(1,4:6)]

#### Merge predicted suitability with abundance data
sdm.suitability_abundance.data=left_join(sdm.data.DOW.latlon,EWM.relfrq.index, by="DOWLKNUM")
sdm.suitability_abundance.naomit=sdm.suitability_abundance.data%>%
na.omit()
sdm.suitability_abundance.naomit

sdm.suitability_abundance.naomit.nonzero=sdm.suitability_abundance.naomit%>%filter(MYS_relfrq>0)
sdm.suitability_abundance.naomit.nonzero

### Make graphs of abundance-suitability relationship


############### Repeat for models with autocovariate term

prsabs.autologistic.sdm=read.sdm("MinnEWMresults/prsabs.ac.SDM.sdm")
randpseuabs.autologistic.sdm=read.sdm("MinnEWMresults/randpseuabs.ac.SDM.sdm")
outrand.pseuabs.autologistic.sdm=read.sdm("MinnEWMresults/outrandpseuabs.ac.SDM.sdm")
nearrand.pseuabs.autologistic.sdm=read.sdm("MinnEWMresults/nearrandpseuabs.ac.SDM.sdm")

head(sdm.ac.data.df)
dim(sdm.ac.data.df)
sdm.ac.data.df.naomit=sdm.ac.data.df%>%na.omit()
head(sdm.ac.data.df.naomit)
dim(sdm.ac.data.df.naomit)

sdm.ac.preds.data=sdm.ac.data.df.naomit[,-c(1:3)]
dim(sdm.ac.preds.data)
head(sdm.ac.preds.data)
predict.sdm.ac.rf=predict(prsabs.autologistic.sdm, newdata=sdm.ac.preds.data, method="rf", mean=TRUE)
predict.sdm.ac.rf.df=as.data.frame(predict.sdm.ac.rf)
tail(predict.sdm.ac.rf.df$`sp_1-m_rf-re_subs`)

sdm.ac.data.DOW.utms=EWM.infes_relfrq.Conn.preds.prsabs.naomit[,c(1:3,7)]
sdm.ac.data.DOW.utms
sdm.ac.data.DOW.utms$predvalues=predict.sdm.ac.rf.df$`sp_1-m_rf-re_subs`
sdm.ac.data.DOW.utms

colnames(sdm.ac.preds.data)[12]="randpseuabs.ac"
colnames(sdm.ac.preds.data)
predict.sdm.rand.ac.rf=predict(randpseuabs.autologistic.sdm, newdata=sdm.ac.preds.data, method="rf", mean=TRUE)
predict.sdm.rand.ac.rf.df=as.data.frame(predict.sdm.rand.ac.rf)
tail(predict.sdm.rand.ac.rf.df$`sp_1-m_rf-re_subs`)

sdm.ac.data.DOW.utms$rand.predvalues=predict.sdm.rand.ac.rf.df$`sp_1-m_rf-re_subs`
sdm.ac.data.DOW.utms

colnames(sdm.ac.preds.data)[12]="outrand.pseuabs.ac"
predict.sdm.outrand.ac.rf=predict(outrand.pseuabs.autologistic.sdm, newdata=sdm.ac.preds.data, method="rf", mean=TRUE)
predict.sdm.outrand.ac.rf.df=as.data.frame(predict.sdm.outrand.ac.rf)
tail(predict.sdm.outrand.ac.rf.df$`sp_1-m_rf-re_subs`)
sdm.ac.data.DOW.utms$outrand.predvalues=predict.sdm.outrand.ac.rf.df$`sp_1-m_rf-re_subs`
sdm.ac.data.DOW.utms


colnames(sdm.ac.preds.data)[12]="nearrand.pseuabs.ac"
predict.sdm.nearrand.ac.rf=predict(nearrand.pseuabs.autologistic.sdm, newdata=sdm.ac.preds.data, method="rf", mean=TRUE)
predict.sdm.nearrand.ac.rf.df=as.data.frame(predict.sdm.nearrand.ac.rf)
tail(predict.sdm.nearrand.ac.rf.df$`sp_1-m_rf-re_subs`)
sdm.ac.data.DOW.utms$nearrand.predvalues=predict.sdm.nearrand.ac.rf.df$`sp_1-m_rf-re_subs`
sdm.ac.data.DOW.utms

sdm.ac_suitability_abundance.data=left_join(sdm.ac.data.DOW.utms,EWM.relfrq.index, by="DOWLKNUM")
sdm.ac_suitability_abundance.data
sdm.ac_suitability_abundance.data.na.omit=sdm.ac_suitability_abundance.data%>%
na.omit()
sdm.ac_suitability_abundance.data.na.omit

plot(sdm.ac_suitability_abundance.data.na.omit$predvalues,sdm.ac_suitability_abundance.data.na.omit$MYS_relfrq)
cor.test(sdm.ac_suitability_abundance.data.na.omit$predvalues,sdm.ac_suitability_abundance.data.na.omit$MYS_relfrq)

sdm.ac_suitability_abundance.data.na.omit.nonzero=sdm.ac_suitability_abundance.data.na.omit%>%
filter(MYS_relfrq>0)
sdm.ac_suitability_abundance.data.na.omit.nonzero

plot(sdm.ac_suitability_abundance.data.na.omit.nonzero$predvalues,sdm.ac_suitability_abundance.data.na.omit.nonzero$MYS_relfrq)
cor.test(sdm.ac_suitability_abundance.data.na.omit.nonzero$predvalues,sdm.ac_suitability_abundance.data.na.omit.nonzero$MYS_relfrq)


#################Combine suitaibility abundance dataset for both models with and without autocovariate
sdm.suitability_abundance.naomit
sdm.suitability_abundance.naomit.trim=sdm.suitability_abundance.naomit%>%
dplyr::select(-c(LON.x,LAT.x))
sdm.ac.suitability_abundance.naomit=sdm.ac_suitability_abundance.data.na.omit%>%
dplyr::select(-c(UTMX,UTMY))
sdm.suitability_abundance.naomit.trim
sdm.ac.suitability_abundance.naomit

col_names=c("DOWLKNUM", "EWMSTATUS_corrRelFrq", "predvalues", "rand.predvalues", "outrand.predvalues", "nearrand.predvalues",
            "LON", "LAT", "MYS_relfrq")
colnames(sdm.suitability_abundance.naomit.trim)=col_names
colnames(sdm.suitability_abundance.naomit.trim)
sdm.suitability_abundance.naomit.trim
sdm.ac.suitability_abundance.naomit
sdm.suitability_abundance.naomit.trim$Autocovariate=rep("absent", 392)
sdm.ac.suitability_abundance.naomit$Autocovariate=rep("inv.dist", 392)

sdm.suitability_abundance.ac_nonac.combined=bind_rows(sdm.suitability_abundance.naomit.trim,sdm.ac.suitability_abundance.naomit)
sdm.suitability_abundance.ac_nonac.combined
wedgeplot_abs= ggscatter(sdm.suitability_abundance.ac_nonac.combined, x = "predvalues", y = "MYS_relfrq", color="Autocovariate", 
                         palette = "RdBlu", shape=1, xlab = "Predicted suitability", ylab = "Occurrence frequency")+
                          theme_classic(base_size = 22)+theme(legend.position = c(0.3,0.85))+
                          stat_cor(aes(color = Autocovariate), label.y.npc = "bottom")
wedgeplot_abs
png("MinnEWMfigures/Suit_Abund.abs.png",units="in", width=5, height=4, res=900)
wedgeplot_abs
dev.off()

wedgeplot_randabs= ggscatter(sdm.suitability_abundance.ac_nonac.combined, x = "rand.predvalues", y = "MYS_relfrq", color="Autocovariate", 
                         palette = "RdBlu", shape=1, xlab = "Predicted suitability", ylab = "Occurrence frequency")+
  theme_classic(base_size = 22)+theme(legend.position = "none")
png("MinnEWMfigures/Suit_Abund.randabs.png",units="in", width=5, height=4, res=900)
wedgeplot_randabs
dev.off()

wedgeplot_outrandabs= ggscatter(sdm.suitability_abundance.ac_nonac.combined, x = "outrand.predvalues", y = "MYS_relfrq", color="Autocovariate", 
                             palette = "RdBlu", shape=1, xlab = "Predicted suitability", ylab = "Occurrence frequency")+
  theme_classic(base_size = 22)+theme(legend.position = "none")

png("MinnEWMfigures/Suit_Abund.dist_randabs.png",units="in", width=5, height=4, res=900)
wedgeplot_outrandabs
dev.off()

wedgeplot_nearrandabs= ggscatter(sdm.suitability_abundance.ac_nonac.combined, x = "nearrand.predvalues", y = "MYS_relfrq", color="Autocovariate", 
                                palette = "RdBlu", shape=1, xlab = "Predicted suitability", ylab = "Occurrence frequency")+
  theme_classic(base_size = 22)+theme(legend.position = "none")
png("MinnEWMfigures/Suit_Abund.proximal_randabs.png",units="in", width=5, height=4, res=900)
wedgeplot_nearrandabs
dev.off()
