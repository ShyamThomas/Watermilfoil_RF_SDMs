########################### DOCUMENTING CODES FOR FIGURES 2 & 3IN EWM NICHE MODELING MANUSCRIPT
######################## All codes written by Shyam Thomas

library(ggplot2)
library(tidyverse)
library(sdm)

#### Figure 2 Relative variable importance: a-e
## (a) Surveyed presence absences
prsabs.sdm=read.sdm("MinnEWMresults/prsabs.SDM.sdm")
prsabs.sdm
rf.varimp=getVarImp(prsabs.sdm, method='rf')
varimp.rf.abs=rf.varimp@varImportanceMean$AUCtest
varimp.rf.abs$ModelType=rep("Absence",11)
varimp.rf.abs
varimp.rf.abs$variables

variables.renamed=c("Max. depth", "Lake area", "pH", "Secchi depth", "Conductance", "Phosphorus", "Chlorophyll-A", "CDOM-a440", "GDD-10cWtr",
"Stream density", "Road density")
varimp.rf.abs$variables=variables.renamed
varimp.rf.abs
p.vimp.abs=ggplot(data=varimp.rf.abs, aes(x=reorder(variables,AUCtest*100) , y=AUCtest*100)) +
geom_bar(stat="identity", fill="dark gray", color="black", position=position_dodge())+
ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60))
p.vimp.abs+theme_bw(base_size = 22)

png("MinnEWMfigures/SDM.PrsAbs.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.abs+theme_bw(base_size = 22)
dev.off()

## (b) Surveyed presence and random pseudoabsences
randabs.sdm=read.sdm("MinnEWMresults/randpseuabs.SDM.sdm")
randabs.sdm
rf.varimp.randabs =getVarImp(randabs.sdm, method='rf')
varimp.rf.randabs=rf.varimp.randabs@varImportanceMean$AUCtest
varimp.rf.randabs$ModelType=rep("Absence",11)
varimp.rf.randabs
varimp.rf.randabs$variables

varimp.rf.randabs$variables=variables.renamed
p.vimp.randabs=ggplot(data=varimp.rf.randabs, aes(x=reorder(variables,AUCtest*100) , y=AUCtest*100)) +
geom_bar(stat="identity", fill="dark gray", color="black", position=position_dodge())+
ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60))

png("MinnEWMfigures/SDM.RandAbs.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.randabs+theme_bw(base_size = 22)
dev.off()

## (c) Surveyed presences and distant pseudoabsences
outrandabs.sdm=read.sdm("MinnEWMresults/outrandpseuabs.SDM.sdm")
outrandabs.sdm
rf.varimp.outrandabs =getVarImp(outrandabs.sdm, method='rf')
varimp.rf.outrandabs=rf.varimp.outrandabs@varImportanceMean$AUCtest
varimp.rf.outrandabs$ModelType=rep("Absence",11)
varimp.rf.outrandabs
varimp.rf.outrandabs$variables

varimp.rf.outrandabs$variables=variables.renamed
p.vimp.outrandabs=ggplot(data=varimp.rf.outrandabs, aes(x=reorder(variables,AUCtest*100) , y=AUCtest*100)) +
  geom_bar(stat="identity", fill="dark gray", color="black", position=position_dodge())+
  ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60))

p.vimp.outrandabs

png("MinnEWMfigures/SDM.OutRandAbs.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.outrandabs+theme_bw(base_size = 22)
dev.off()

## (d) Surveyed presences and proximal pseudoabsences
nearrandabs.sdm=read.sdm("MinnEWMresults/nearrandpseuabs.SDM.sdm")
nearrandabs.sdm
rf.varimp.nearrandabs =getVarImp(nearrandabs.sdm, method='rf')
varimp.rf.nearrandabs=rf.varimp.nearrandabs@varImportanceMean$AUCtest
varimp.rf.nearrandabs$ModelType=rep("Absence",11)
varimp.rf.nearrandabs
varimp.rf.nearrandabs$variables

varimp.rf.nearrandabs$variables=variables.renamed
p.vimp.nearrandabs=ggplot(data=varimp.rf.nearrandabs, aes(x=reorder(variables,AUCtest*100) , y=AUCtest*100)) +
  geom_bar(stat="identity", fill="dark gray", color="black", position=position_dodge())+
  ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60))

p.vimp.nearrandabs

png("MinnEWMfigures/SDM.NearRandAbs.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.nearrandabs+theme_bw(base_size = 22)
dev.off()

## (e) Abundance model
EWM.infes_relfrq.preds=read_csv("MinnEWMdata/EWMinfes_relfrq.preds.data.csv")


EWM.abun.data.rf=EWM.relfrq.Conn.preds.data.selvars[,-c(1,2)]
EWM.abun.data.rf
abun.sdm.rf=randomForest(MYS_relfrq ~ ., data=EWM.abun.data.rf, ntree=500, keep.forest=TRUE,
importance=TRUE)

abun.vimp.rf <- varImpPlot(abun.sdm.rf)
abun.vimp.rf.df=as.data.frame(abun.vimp.rf)
abun.vimp.rf.df$variable=varimp.rf.abs$variables
p.vimp.abun.rf=ggplot(data=abun.vimp.rf.df, aes(x=reorder(variable,`%IncMSE`), y=`%IncMSE`)) +
geom_bar(stat="identity", fill="white", color="black", position=position_dodge())+
ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim=c(0,60))
p.vimp.abun.rf

png("MinnEWMfigures/SDM.Abun.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.abun.rf+theme_bw(base_size = 22)
dev.off()

############################################################################################################
#### Figure 3. Plotting response curves: (a)--(f)
## (a) Growing degree days response curves
rf.rcurve.prsabs.gdd=rcurve(prsabs.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="GDD", ylab="Invasion risk")
rf.rcurve.randpseuabs.gdd=rcurve(randabs.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                                 ylab="Invasion risk")
rf.rcurve.outrandpseuabs.gdd=rcurve(outrandabs.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                                    ylab="Invasion risk")
rf.rcurve.nearrandpseuabs.gdd=rcurve(nearrandabs.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                                     ylab="Invasion risk")

rf.rcurve.prsabs.gdd$data$Model=rep("Absence", 100)
rf.rcurve.randpseuabs.gdd$data$Model=rep("Random", 100)
rf.rcurve.outrandpseuabs.gdd$data$Model=rep("Distant", 100)
rf.rcurve.nearrandpseuabs.gdd$data$Model=rep("Proximal", 100)

GDD.allmodels.rcurves=rbind(rf.rcurve.prsabs.gdd$data,rf.rcurve.randpseuabs.gdd$data,rf.rcurve.outrandpseuabs.gdd$data,
                            rf.rcurve.nearrandpseuabs.gdd$data)
head(GDD.allmodels.rcurves)

GDD.invrisk.plot=ggplot(GDD.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.title = element_blank())+
  xlab("Growing degree days")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))
GDD.invrisk.plot
GDD.invrisk.plot+theme_bw(base_size = 22)+theme(legend.justification=c(1,0), legend.position=c(0.95,0.01))+
  theme(legend.title = element_blank())

png("MinnEWMfigures/SDM.GDD.RespCurv.png",units="in", width=5, height=4, res=900)
GDD.invrisk.plot+theme_bw(base_size = 22)+theme(legend.justification=c(1,0), legend.position=c(0.95,0.01))+
  theme(legend.title = element_blank())
dev.off()

## (b) Road density response curves
rf.rcurve.prsabs.road=rcurve(prsabs.sdm, id=c(121:180), n="roaddensity_density_mperha", main=" ", xlab="Road density (mpha)", 
                             ylab="Invasion risk")
rf.rcurve.outrandpseuabs.road=rcurve(outrandabs.sdm, id=c(121:180), n="roaddensity_density_mperha", main=" ",
                                     xlab="Road density (mpha)", ylab="Invasion risk")
rf.rcurve.randpseuabs.road=rcurve(randabs.sdm, id=c(121:180), n="roaddensity_density_mperha", main=" ", 
                                  xlab="Road density (mpha)", ylab="Invasion risk")
rf.rcurve.nearrandpseuabs.road=rcurve(nearrandabs.sdm, id=c(121:180), n="roaddensity_density_mperha", main=" ", 
                                      xlab="Road density (mpha)", ylab="Invasion risk")

rf.rcurve.prsabs.road$data$Model=rep("Absence", 100)
rf.rcurve.randpseuabs.road$data$Model=rep("Random", 100)
rf.rcurve.outrandpseuabs.road$data$Model=rep("Distant", 100)
rf.rcurve.nearrandpseuabs.road$data$Model=rep("Proximal", 100)
Road.allmodels.rcurves=rbind(rf.rcurve.prsabs.road$data,rf.rcurve.randpseuabs.road$data,rf.rcurve.outrandpseuabs.road$data,
                             rf.rcurve.nearrandpseuabs.road$data)

Road.invrisk.plot=ggplot(Road.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.position = "NA")+
  xlab("Road density (mpha)")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))

Road.invrisk.plot+theme_bw(base_size = 22)+theme(legend.position="none")

png("MinnEWMfigures/SDM.Roads.RespCurv.png",units="in", width=5, height=4, res=900)
Road.invrisk.plot+theme_bw(base_size = 22)+theme(legend.position="none")
dev.off()


## (c) Lake depth response curves

rf.rcurve.prsabs.depth=rcurve(prsabs.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Lake depth (m)",
                              ylab="Invasion risk")
rf.rcurve.outrandpseuabs.depth=rcurve(outrandabs.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Lake depth (m)",
                                      ylab="Invasion risk")
rf.rcurve.randpseuabs.depth=rcurve(randabs.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Lake depth (m)",
                                   ylab="Invasion risk")
rf.rcurve.nearrandpseuabs.depth=rcurve(nearrandabs.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Lake depth (m)",
                                       ylab="Invasion risk")

rf.rcurve.prsabs.depth$data$Model=rep("Absence", 100)
rf.rcurve.randpseuabs.depth$data$Model=rep("Random", 100)
rf.rcurve.outrandpseuabs.depth$data$Model=rep("Distant", 100)
rf.rcurve.nearrandpseuabs.depth$data$Model=rep("Proximal", 100)
Depth.allmodels.rcurves=rbind(rf.rcurve.prsabs.depth$data,rf.rcurve.randpseuabs.depth$data,rf.rcurve.outrandpseuabs.depth$data,
                              rf.rcurve.nearrandpseuabs.depth$data)

Depth.invrisk.plot=ggplot(Depth.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.position = "NA")+
  xlab("Lake depth (m)")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))

Depth.invrisk.plot+theme_bw(base_size = 22)+theme(legend.position="none")

png("MinnEWMfigures/SDM.Depth.respcurv.png",units="in", width=5, height=4, res=900)
Depth.invrisk.plot+theme_bw(base_size = 22)+theme(legend.position="none")
dev.off()

## (d) Abundance response curve for GDD
rf.rcurve.abun.gdd=rcurve(abun.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days",
                          ylab="Occurrence frequency")
rf.rcurve.abun.gdd
rf.rcurve.abun.gdd.df=as.data.frame(rf.rcurve.abun.gdd$data)


GDD.Abund.resp_plot=ggplot(rf.rcurve.abun.gdd.df, aes(x=Value, y=Response))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  xlab("Growing degree days")+ylab("Occurrence frequency")+coord_cartesian(ylim = c(0.0,0.25))
GDD.Abund.resp_plot+theme_bw(base_size = 22)

png("MinnEWMfigures/Abund.GDD.respcurv.png",units="in", width=5, height=4, res=900)
GDD.Abund.resp_plot+theme_bw(base_size = 22)
dev.off()

## (e) Abundance response curve for lake depth
rf.rcurve.abun.depth=rcurve(abun.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Maximum depth",
                            ylab="Occurrence frequency")
rf.rcurve.abun.depth
rf.rcurve.abun.depth.df=as.data.frame(rf.rcurve.abun.depth$data)

Depth.Abund.resp_plot=ggplot(rf.rcurve.abun.depth.df, aes(x=Value, y=Response))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  xlab("Lake depth (m)")+ylab("Occurrence frequency")+coord_cartesian(ylim = c(0.0,0.25))

png("MinnEWMfigures/Abund.Depth.respcurv.png",units="in", width=5, height=4, res=900)
Depth.Abund.resp_plot+theme_bw(base_size = 22)
dev.off()

## (f) Abundance response curve for lake depth
rf.rcurve.abun.secchi=rcurve(abun.sdm, id=c(121:180), n="avg_secchi", main=" ", xlab="Secchi depth",
                             ylab="Occurrence frequency")
rf.rcurve.abun.secchi
rf.rcurve.abun.secchi.df=as.data.frame(rf.rcurve.abun.secchi$data)

Secchi.Abund.resp_plot=ggplot(rf.rcurve.abun.secchi.df, aes(x=Value, y=Response))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  xlab("Secchi depth (m)")+ylab("Occurrence frequency")+coord_cartesian(ylim = c(0.0,0.25))

png("MinnEWMfigures/Abund.Secchi.respcurv.png",units="in", width=5, height=4, res=900)
Secchi.Abund.resp_plot+theme_bw(base_size = 22)
dev.off()

