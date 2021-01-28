############################################# Incorporating Spatial Lag term: Autocovariate driven SDMs #################################
setwd("~/UMNpostdoc/ProjectEWM/MinnEWM")

library(tidyverse)
library(sdm)
library(sp)
library(spdep)

############################# Getting the data: repeat of first 20 lines of codes from previous non-spatial SDMs
############## SDMs for EWM presence-ABSENCE data with AUTOCOVARIATE term
EWM.infes_relfrq.selpreds.prsabs=read_csv("MinnEWMdata/EWM.infes_relfrq.selpreds.prsabs.csv")
EWM.infes_relfrq.selpreds.prsabs
############ Lets now also include Landscape Connectivity  variables: stream and road density
LakeIndex.ConnData = read_csv("MinnEWMdata/LakeConnectivityData.Reduced.csv")
colnames(LakeIndex.ConnData)
LakeConn=LakeIndex.ConnData%>%
  dplyr::select(c(DOWLKNUM, allstreams_density_mperha, roaddensity_density_mperha))
LakeConn
EWM.infes_relfrq.Conn.preds.prsabs=left_join(EWM.infes_relfrq.selpreds.prsabs,LakeConn, by="DOWLKNUM")
EWM.infes_relfrq.Conn.preds.prsabs

sdm.data.utm=EWM.infes_relfrq.Conn.preds.prsabs[,-c(1,4:6)]
sdm.data.utm.df=as.data.frame(sdm.data.utm)
head(sdm.data.utm.df) ### --> Our SDM data is finally ready; 
### Unlike previous SDM data, I retained UTMs instead of LAT/LON since spatial lags in meters make easy interpretation
sdm.data.utm.sf=st_as_sf(sdm.data.utm.df, coords=c("UTMX","UTMY"), crs=32615) ### get the CRS from spatial reference
sdm.data.utm.sf
#### Search radius was set 93000 m (93 kms) through repeated trials
prsabs.ac=autocov_dist(sdm.data.utm.sf$EWMSTATUS_corrRelFrq, sdm.data.utm.sf, nbs = 93000, longlat = FALSE)
head(prsabs.ac, n=8) ### autocovariate term as inverse distance spatial lag
head(1/prsabs.ac, n=8) ### this gives a distance specific glimpse in meters
plot(1/prsabs.ac,sdm.data.utm.df$EWMSTATUS_corrRelFrq)

sdm.ac.data.df=cbind(sdm.data.utm.df, prsabs.ac)
head(sdm.ac.data.df)
dim(sdm.ac.data.df)

SDM.ac.data=sdmData(EWMSTATUS_corrRelFrq~.+coords(UTMX+UTMY),train=sdm.ac.data.df, crs=32615)
SDM.ac.data
plot(SDM.ac.data)

prsabs.autologistic.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.ac.data,methods=c('glm','gam','rf','maxlike'), 
                 replication='sub',test.percent=30,n=60,ncore=12)
write.sdm(prsabs.autologistic.sdm, "MinnEWMresults/prsabs.ac.SDM", overwrite = TRUE)

splitTable = function(df, prob) {
  set.seed(077)
  variant = sample(seq(1, 0), size = nrow(df), replace = TRUE, prob = c(prob, 1 - prob))
  res = split(df, variant)
  return(res)
}

SDM.ac.prsabs.splitdata=splitTable(sdm.ac.data.df,0.7)
sdm.ac.prsabs.traindata=SDM.ac.prsabs.splitdata$'1'
dim(sdm.ac.prsabs.traindata)
head(sdm.ac.prsabs.traindata)
sdm.ac.prsabs.testdata=SDM.ac.prsabs.splitdata$'0'
dim(sdm.ac.prsabs.testdata)
head(sdm.ac.prsabs.testdata)

SDM.ac.prsabs.splitdata=sdmData(EWMSTATUS_corrRelFrq~.+coords(UTMX+UTMY),train=sdm.ac.prsabs.traindata,
                                      test= sdm.ac.prsabs.testdata, crs=32615)

SDM.ac.prsabs.splitdata
plot(SDM.ac.prsabs.splitdata)

split.prsabs.autologistic.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.ac.prsabs.splitdata,methods=c('glm','gam','rf','maxlike'))
split.prsabs.autologistic.sdm
write.sdm(split.prsabs.autologistic.sdm, "MinnEWMresults/split.prsabs.ac.SDM.sdm")

######################################################################################################################################
######################################################################################################################################
##### RANDOM Pseudo-absences based SDMs with AUTOCOVARIATE term
EWM.infes_relfrq.selpreds.prs.randpseuabs=read_csv("MinnEWMdata/Rand.pseuabs.sdm.csv")
EWM.infes_relfrq.selpreds.prs.randpseuabs
EWM.infes_relfrq.Conn.preds.prs.randpseuabs=left_join(EWM.infes_relfrq.selpreds.prs.randpseuabs,LakeConn, by="DOWLKNUM")
EWM.infes_relfrq.Conn.preds.prs.randpseuabs
sdm.randpseuabs.data=EWM.infes_relfrq.Conn.preds.prs.randpseuabs[,-c(1,4:6)]%>%
na.omit()
sdm.randpseuabs.data
sdm.randpseuabs.df=as.data.frame(sdm.randpseuabs.data)
head(sdm.randpseuabs.df)
dim(sdm.randpseuabs.df)

sdm.randpseuabs.data.utm.sf=st_as_sf(sdm.randpseuabs.df, coords=c("UTMX","UTMY"), crs=32615)
sdm.randpseuabs.data.utm.sf

randpseuabs.ac=autocov_dist(sdm.randpseuabs.data.utm.sf$EWMSTATUS_corrRelFrq, sdm.randpseuabs.data.utm.sf, nbs = 135000, longlat = FALSE)
head(randpseuabs.ac)
plot(1/randpseuabs.ac,sdm.randpseuabs.df$EWMSTATUS_corrRelFrq)
sdm.ac.randpseuabs.data=cbind(sdm.randpseuabs.df, randpseuabs.ac)
head(sdm.ac.randpseuabs.data)
dim(sdm.ac.randpseuabs.data)

SDM.ac.randpseuabs.data=sdmData(EWMSTATUS_corrRelFrq~.+coords(UTMX+UTMY),train=sdm.ac.randpseuabs.data, crs=32615)
randpseuabs.autologistic.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.ac.randpseuabs.data,methods=c('glm','gam','rf','maxlike'),
replication='sub',test.percent=30,n=60,ncore=12)
randpseuabs.autologistic.sdm

splitTable = function(df, prob) {
set.seed(077)
variant = sample(seq(1, 0), size = nrow(df), replace = TRUE, prob = c(prob, 1 - prob))
res = split(df, variant)
return(res)
}

SDM.ac.rand.pseuabs.splitdata=splitTable(sdm.ac.randpseuabs.data,0.7)
sdm.ac.randpseuabs.traindata=sdm.ac.randpseuabs.data.split$'1'
dim(sdm.ac.randpseuabs.traindata)
head(sdm.ac.randpseuabs.traindata)
sdm.ac.randpseuabs.testdata=sdm.ac.randpseuabs.data.split$'0'
dim(sdm.ac.randpseuabs.testdata)
head(sdm.ac.randpseuabs.testdata)

SDM.ac.rand.pseuabs.splitdata=sdmData(EWMSTATUS_corrRelFrq~.+coords(UTMX+UTMY),train=sdm.ac.randpseuabs.traindata,
test= sdm.ac.randpseuabs.testdata, crs=32615)

SDM.ac.rand.pseuabs.splitdata
plot(SDM.ac.rand.pseuabs.splitdata)

split.randpseuabs.autologistic.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.ac.rand.pseuabs.splitdata,methods=c('glm','gam','rf','maxlike'))
split.randpseuabs.autologistic.sdm
write.sdm(split.randpseuabs.autologistic.sdm, "MinnEWMresults/split.randpseuabs.ac.SDM", overwrite = TRUE)

######################################################################################################################################
##### DISTANT Pseudo-absences based SDMs with AUTOCOVARIATE term
EWM.infes_relfrq.selpreds.prs.outrand.pseuabs=read_csv("MinnEWMdata/OutRand.pseuabs.sdm.csv")
EWM.infes_relfrq.selpreds.prs.outrand.pseuabs
EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs=left_join(EWM.infes_relfrq.selpreds.prs.outrand.pseuabs,LakeConn, by="DOWLKNUM")
EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs
EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs%>%
na.omit()
sdm.outrandpseuabs.data=EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs[,-c(1:4)]
sdm.outrandpseuabs.data
sdm.outrand.pseuabs.df=as.data.frame(sdm.outrandpseuabs.data)

sdm.outrand.pseuabs.data.utm.sf=st_as_sf(sdm.outrand.pseuabs.df, coords=c("UTMX","UTMY"), crs=32615)
sdm.outrand.pseuabs.data.utm.sf
outrand.pseuabs.ac=autocov_dist(sdm.outrand.pseuabs.data.utm.sf$EWMSTATUS_corrRelFrq, sdm.outrand.pseuabs.data.utm.sf, nbs = 93000, longlat = FALSE)
head(outrand.pseuabs.ac)

plot(1/outrand.pseuabs.ac,sdm.outrand.pseuabs.df$EWMSTATUS_corrRelFrq)
sdm.ac.outrand.pseuabs.data=cbind(sdm.outrand.pseuabs.df, outrand.pseuabs.ac)
head(sdm.ac.outrand.pseuabs.data)

SDM.ac.outrandpseuabs.data=sdmData(EWMSTATUS_corrRelFrq~.+coords(UTMX+UTMY),train=sdm.ac.outrand.pseuabs.data, crs=32615)
plot(SDM.ac.outrandpseuabs.data)
outrandpseuabs.autologistic.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.ac.outrandpseuabs.data,methods=c('glm','gam','rf','maxlike'),
replication='sub',test.percent=30,n=60,ncore=12)


sdm.ac.outrand.pseuabs.data.split=splitTable(sdm.ac.outrand.pseuabs.data,0.7)
sdm.ac.outrand.pseuabs.traindata=sdm.ac.outrand.pseuabs.data.split$'1'
dim(sdm.ac.outrand.pseuabs.traindata)
head(sdm.ac.outrand.pseuabs.traindata)
sdm.ac.outrand.pseuabs.testdata=sdm.ac.outrand.pseuabs.data.split$'0'
dim(sdm.ac.outrand.pseuabs.testdata)
head(sdm.ac.outrand.pseuabs.testdata)

SDM.ac.outrand.pseuabs.splitdata=sdmData(EWMSTATUS_corrRelFrq~.+coords(UTMX+UTMY),train=sdm.ac.outrand.pseuabs.traindata,
                                    test= sdm.ac.outrand.pseuabs.testdata,crs=32615)
SDM.ac.outrand.pseuabs.splitdata
plot(SDM.ac.outrand.pseuabs.splitdata)
split.outrand.pseuabs.autologistic.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.ac.outrand.pseuabs.splitdata,methods=c('glm','gam','rf','maxlike'))
split.outrand.pseuabs.autologistic.sdm

######################################################################################################################################
##### PROXIMAL Pseudo-absences based SDMs with AUTOCOVARIATE term

EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs=read_csv("MinnEWMdata/NearRand.pseuabs.sdm.csv")
EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs
EWM.infes_relfrq.Conn.preds.prs.nearrand.pseuabs=left_join(EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs,LakeConn, by="DOWLKNUM")
EWM.infes_relfrq.Conn.preds.prs.nearrand.pseuabs

sdm.nearrandpseuabs.data=EWM.infes_relfrq.Conn.preds.prs.nearrand.pseuabs[,-c(1:4)]
sdm.nearrand.pseuabs.df=as.data.frame(sdm.nearrandpseuabs.data)
head(sdm.nearrand.pseuabs.df)

sdm.nearrand.pseuabs.data.utm.sf=st_as_sf(sdm.nearrand.pseuabs.df, coords=c("UTMX","UTMY"), crs=32615)
sdm.nearrand.pseuabs.data.utm.sf
nearrand.pseuabs.ac=autocov_dist(sdm.nearrand.pseuabs.data.utm.sf$EWMSTATUS_corrRelFrq, sdm.nearrand.pseuabs.data.utm.sf, nbs = 31000, 
                                 longlat = FALSE)
plot(1/nearrand.pseuabs.ac,sdm.nearrand.pseuabs.df$EWMSTATUS_corrRelFrq)
sdm.ac.nearrand.pseuabs.data=cbind(sdm.nearrand.pseuabs.df, nearrand.pseuabs.ac)
head(sdm.ac.nearrand.pseuabs.data)

SDM.ac.nearrandpseuabs.data=sdmData(EWMSTATUS_corrRelFrq~.+coords(UTMX+UTMY),train=sdm.ac.nearrand.pseuabs.data, crs=32615)
SDM.ac.nearrandpseuabs.data
plot(SDM.ac.nearrandpseuabs.data)

nearrandpseuabs.autologistic.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.ac.nearrandpseuabs.data,methods=c('glm','gam','rf','maxlike'),
replication='sub',test.percent=30,n=60,ncore=12)
nearrandpseuabs.autologistic.sdm
write.sdm(nearandpseuabs.autologistic.sdm, "MinnEWMresults/nearrandpseuabs.ac.SDM", overwrite = TRUE)

### Split and make an quasi-independent test data for validation
sdm.ac.nearrand.pseuabs.data.split=splitTable(sdm.ac.nearrand.pseuabs.data,0.7)
sdm.ac.nearrand.pseuabs.traindata=sdm.ac.nearrand.pseuabs.data.split$'1'
dim(sdm.ac.nearrand.pseuabs.traindata)
head(sdm.ac.nearrand.pseuabs.traindata)
sdm.ac.nearrand.pseuabs.testdata=sdm.ac.nearrand.pseuabs.data.split$'0'
dim(sdm.ac.nearrand.pseuabs.testdata)
head(sdm.ac.nearrand.pseuabs.testdata)

SDM.ac.nearrand.pseuabs.splitdata=sdmData(EWMSTATUS_corrRelFrq~.+coords(UTMX+UTMY),train=sdm.ac.nearrand.pseuabs.traindata,
test=sdm.ac.nearrand.pseuabs.testdata, crs=32615)

SDM.ac.nearrand.pseuabs.splitdata
plot(SDM.ac.nearrand.pseuabs.splitdata)

split.nearrand.pseuabs.autologistic.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.ac.nearrand.pseuabs.splitdata,
                                              methods=c('glm','gam','rf','maxlike'))

write.sdm(split.nearrand.pseuabs.autologistic.sdm, "MinnEWMresults/split.nearrandpseuabs.ac.SDM", overwrite = TRUE)

######################################################################################################################################
######################################################################################################################################
##### ABUNDANCE based SDMS with AUTOCOVARIATE term

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
EWM.relfrq.Conn.preds.data
EWM.relfrq.Conn.preds.data.selvars=EWM.relfrq.Conn.preds.data%>%
dplyr::select(-c(avg_totalalkalinity, avg_nitrogen,  DOWLKNUM))%>%
na.omit()
EWM.relfrq.Conn.preds.data.selvars ### Data with EWM relative frequency measure of abundance with UTMs

sdm.abund.data=as.data.frame(EWM.relfrq.Conn.preds.data.selvars)
head(sdm.abund.data)
sdm.abund.data.utm.sf=st_as_sf(sdm.abund.data, coords=c("UTMX","UTMY"), crs=32615)
sdm.abund.data.utm.sf
abund.ac=autocov_dist(sdm.abund.data.utm.sf$MYS_relfrq, sdm.abund.data.utm.sf, nbs = 93000, longlat = FALSE)
head(abund.ac)
head(1/abund.ac)
plot(abund.ac, sdm.abund.data$MYS_relfrq)

sdm.abund.ac.data=cbind(sdm.abund.data[-c(3,4)],abund.ac)
head(sdm.abund.ac.data)
dim(sdm.abund.ac.data)
SDM.abund.ac.data=sdmData(MYS_relfrq~.+coords(UTMX+UTMY),train=sdm.abund.ac.data, crs=32615)
SDM.abund.ac.data
abund.autocov.sdm = sdm(MYS_relfrq~.,data=SDM.abund.ac.data,methods=c('glm','gam','rf','maxlike'),
replication='sub',test.percent=30,n=60,ncore=12)

abund.autocov.sdm
write.sdm(abund.autocov.sdm, "MinnEWMresults/abun.ac.SDM.sdm", overwrite = TRUE)


### A random forest model done seperately
EWM.abun.autocov.data.rf=sdm.abund.ac.data[,-c(1,2)]
head(EWM.abun.autocov.data.rf)
abun.autocov.sdm.rf=randomForest(MYS_relfrq ~ ., data=EWM.abun.autocov.data.rf, ntree=500, keep.forest=TRUE,
                         importance=TRUE)
abun.autocov.sdm.rf
saveRDS(abun.autocov.sdm.rf,"MinnEWMresults/Abund.autocov.rf.rds")
######################################################################################################################################
######################################################################################################################################
########################## All DISTANCE WEIGHTED AUTOCOVARIATE models so far... 
### Autologistic models
prsabs.autologistic.sdm
randpseuabs.autologistic.sdm
outrand.pseuabs.autologistic.sdm
nearrand.pseuabs.autologistic.sdm

abund.autocov.sdm
###[1] Relative Variable Importance
rf.varimp.prsabs.autocov=getVarImp(prsabs.autologistic.sdm, method='rf')
varimp.rf.prsabs.autocov=rf.varimp.prsabs.autocov@varImportanceMean$AUCtest
varimp.rf.prsabs.autocov$ModelType=rep("Absence",12)
varimp.rf.prsabs.autocov

p.vimp.prsabs.ac=ggplot(data=varimp.rf.prsabs.autocov, aes(x=reorder(variables,AUCtest*100) , y=AUCtest*100)) +
  geom_bar(stat="identity", fill="dark gray", color="black", position=position_dodge())+
  ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60)) 

png("MinnEWMfigures/SDM.PrsAbs.AutoCov.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.prsabs.ac+theme_bw(base_size = 22)
dev.off()

abun.autocov.vimp.rf = varImpPlot(abun.autocov.sdm.rf)
abun.autocov.vimp.rf.df=as.data.frame(abun.autocov.vimp.rf)
abun.autocov.vimp.rf.df$variable=varimp.rf.prsabs.autocov$variables


p.vimp.abun.autocov.rf=ggplot(data=abun.autocov.vimp.rf.df, aes(x=reorder(variable,`%IncMSE`), y=`%IncMSE`)) +
  geom_bar(stat="identity", fill="white", color="black", position=position_dodge())+
  ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim=c(0,60))

png("MinnEWMfigures/SDM.Abun.AutoCov.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.abun.autocov.rf
dev.off()

#### [2] Plotting response curves
## Only 3 most important variables were chosen besides the Autocovariate term: GDD, road density, and max depth
## (1) Growing degree days response curves
rf.rcurve.prsabs.ac.gdd=rcurve(prsabs.autologistic.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="GDD", ylab="Invasion risk")
rf.rcurve.randpseuabs.ac.gdd=rcurve(randpseuabs.autologistic.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                                 ylab="Invasion risk")
rf.rcurve.outrandpseuabs.ac.gdd=rcurve(outrand.pseuabs.autologistic.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                                    ylab="Invasion risk")
rf.rcurve.nearrandpseuabs.ac.gdd=rcurve(nearrand.pseuabs.autologistic.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                                     ylab="Invasion risk")

rf.rcurve.prsabs.ac.gdd$data$Model=rep("Absence", 100)
rf.rcurve.randpseuabs.ac.gdd$data$Model=rep("Pseudoabs_Rand", 100)
rf.rcurve.outrandpseuabs.ac.gdd$data$Model=rep("Pseudoabs_Dist", 100)
rf.rcurve.nearrandpseuabs.ac.gdd$data$Model=rep("Pseudoabs_Near", 100)

GDD.ac.allmodels.rcurves=rbind(rf.rcurve.prsabs.ac.gdd$data,rf.rcurve.randpseuabs.ac.gdd$data,rf.rcurve.outrandpseuabs.ac.gdd$data,
                            rf.rcurve.nearrandpseuabs.ac.gdd$data)
head(GDD.ac.allmodels.rcurves)

GDD.ac.invrisk.plot=ggplot(GDD.ac.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.title = element_blank())+
  xlab("Growing degree days")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))

#### For abundace data
rf.rcurve.abun.ac.gdd=rcurve(abund.autocov.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="GDD", ylab="Invasion risk")
rf.rcurve.abun.ac.gdd
rf.rcurve.abun.ac.gdd.df=as.data.frame(rf.rcurve.abun.ac.gdd$data)
head(rf.rcurve.abun.ac.gdd.df)

GDD.ac.abund.plot=ggplot(rf.rcurve.abun.ac.gdd.df, aes(x=Value, y=Response))+
geom_line(lwd=1)+
geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
xlab("Growing degree days")+ylab("Occurrence frequency")+coord_cartesian(ylim = c(0.0,0.20))
GDD.ac.abund.plot

#### [2] Plotting the Autocovariate response
rf.rcurve.prsabs.ac=rcurve(prsabs.autologistic.sdm, id=c(121:180), n="prsabs.ac", main=" ", xlab="Inverse distance", ylab="Invasion risk")
rf.rcurve.randpseuabs.ac=rcurve(randpseuabs.autologistic.sdm, id=c(121:180), n="randpseuabs.ac", main=" ", xlab="Inverse distance", 
                                    ylab="Invasion risk")
rf.rcurve.outrandpseuabs.ac=rcurve(outrand.pseuabs.autologistic.sdm, id=c(121:180), n="outrand.pseuabs.ac", main=" ", xlab="Inverse distance", 
                                       ylab="Invasion risk")
rf.rcurve.nearrandpseuabs.ac=rcurve(nearrand.pseuabs.autologistic.sdm, id=c(121:180), n="nearrand.pseuabs.ac", main=" ", xlab="Inverse distance", 
                                        ylab="Invasion risk")

rf.rcurve.prsabs.ac$data$Model=rep("Absence", 100)
rf.rcurve.randpseuabs.ac$data$Model=rep("Pseudoabs_Rand", 100)
rf.rcurve.outrandpseuabs.ac$data$Model=rep("Pseudoabs_Dist", 100)
rf.rcurve.nearrandpseuabs.ac$data$Model=rep("Pseudoabs_Near", 100)

AC.allmodels.rcurves=rbind(rf.rcurve.prsabs.ac$data,rf.rcurve.randpseuabs.ac$data,rf.rcurve.outrandpseuabs.ac$data,
                               rf.rcurve.nearrandpseuabs.ac$data)
head(AC.allmodels.rcurves)

AC.invrisk.plot=ggplot(AC.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.title = element_blank())+
  xlab("Inverse distance")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))

### For abundance data
rf.rcurve.abun.ac=rcurve(abund.autocov.sdm, id=c(121:180), n="abund.ac", main=" ", xlab="GDD", ylab="Invasion risk")
rf.rcurve.abun.ac.df=as.data.frame(rf.rcurve.abun.ac$data)
head(rf.rcurve.abun.ac.df)
AC.abund.plot=ggplot(rf.rcurve.abun.ac.df, aes(x=Value, y=Response))+

png("MinnEWMfigures/AbunSDM.Autocov.respcurv.png",units="in", width=4.5, height=4, res=900)
AC.abund.plot
dev.off()


