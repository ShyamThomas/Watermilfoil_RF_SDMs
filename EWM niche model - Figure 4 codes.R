########################### DOCUMENTING CODES FOR FIGURE 4 IN EWM NICHE MODELING MANUSCRIPT
######################## All codes written by Shyam Thomas

library(ggplot2)
library(tidyverse)
library(sdm)

#### Figure 4 Effect of spatial lag term on Relative Variable Importance and GDD response curve: a-d
## Need all the sdm models with autocovariate term
prsabs.autologistic.sdm=read.sdm("MinnEWMresults/prsabs.ac.SDM.sdm")
randpseuabs.autologistic.sdm=read.sdm("MinnEWMresults/randpseuabs.ac.SDM.sdm")
outrand.pseuabs.autologistic.sdm=read.sdm("MinnEWMresults/outrandpseuabs.ac.SDM.sdm")
nearrand.pseuabs.autologistic.sdm=read.sdm("MinnEWMresults/nearrandpseuabs.ac.SDM.sdm")

## (a) Relative Variable Importance of surveyed presence absences with autocovariate term
rf.varimp.prsabs.autocov=getVarImp(prsabs.autologistic.sdm, method='rf')
varimp.rf.prsabs.autocov=rf.varimp.prsabs.autocov@varImportanceMean$AUCtest
varimp.rf.prsabs.autocov$ModelType=rep("Absence",12)
varimp.rf.prsabs.autocov
Autocov.variables.renamed=c("Max. depth", "Lake area", "pH", "Secchi depth", "Conductance", "Phosphorus", "Chlorophyll-A", 
                            "CDOM-a440", "GDD-10cWtr", "Stream density", "Road density","Autocovariate")
varimp.rf.prsabs.autocov$variables=Autocov.variables.renamed
varimp.rf.prsabs.autocov
p.vimp.prsabs.ac=ggplot(data=varimp.rf.prsabs.autocov, aes(x=reorder(variables,AUCtest*100) , y=AUCtest*100)) +
geom_bar(stat="identity", fill="dark gray", color="black", position=position_dodge())+
ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60))

png("MinnEWMfigures/SDM.PrsAbs.AutoCov.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.prsabs.ac+theme_bw(base_size = 22)
dev.off()

## (b) Autocovariate response plots for all prsence absence sdms
rf.rcurve.prsabs.ac=rcurve(prsabs.autologistic.sdm, id=c(121:180), n="prsabs.ac", main=" ", xlab="Inverse distance", ylab="Invasion risk")
rf.rcurve.randpseuabs.ac=rcurve(randpseuabs.autologistic.sdm, id=c(121:180), n="randpseuabs.ac", main=" ", xlab="Inverse distance", 
                                ylab="Invasion risk")
rf.rcurve.outrandpseuabs.ac=rcurve(outrand.pseuabs.autologistic.sdm, id=c(121:180), n="outrand.pseuabs.ac", main=" ", xlab="Inverse distance", 
                                   ylab="Invasion risk")
rf.rcurve.nearrandpseuabs.ac=rcurve(nearrand.pseuabs.autologistic.sdm, id=c(121:180), n="nearrand.pseuabs.ac", main=" ", xlab="Inverse distance", 
                                    ylab="Invasion risk")

rf.rcurve.prsabs.ac$data$Model=rep("Absence", 100)
rf.rcurve.randpseuabs.ac$data$Model=rep("Random", 100)
rf.rcurve.outrandpseuabs.ac$data$Model=rep("Distant", 100)
rf.rcurve.nearrandpseuabs.ac$data$Model=rep("Proximal", 100)

AC.allmodels.rcurves=rbind(rf.rcurve.prsabs.ac$data,rf.rcurve.randpseuabs.ac$data,rf.rcurve.outrandpseuabs.ac$data,
                           rf.rcurve.nearrandpseuabs.ac$data)

AC.invrisk.plot=ggplot(AC.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.title = element_blank())+
  xlab("Inverse distance")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))

AC.invrisk.plot+theme_bw(base_size = 22)+theme(legend.justification=c(1,0), legend.position=c(0.95,0.01))+
  theme(legend.title = element_blank())

png("MinnEWMfigures/SDM_autocov.AC.RespCurv.png",units="in", width=5, height=4, res=900)
AC.invrisk.plot+theme_bw(base_size = 22)+theme(legend.justification=c(1,0), legend.position=c(0.95,0.01))+
  theme(legend.title = element_blank())
dev.off()

## (c) Relative Variable Importance of surveyed presence absences with autocovariate term
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
GDD.ac.invrisk.plot=ggplot(GDD.ac.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
  geom_line(lwd=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
  scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.title = element_blank())+
  xlab("Growing degree days")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))

png("MinnEWMfigures/SDM_autocov.GDD.RespCurv.png",units="in", width=5, height=4, res=900)
GDD.ac.invrisk.plot+theme_bw(base_size = 22)+theme(legend.position="none")
dev.off()

### (d) Relative Variable Importance of surveyed abundance with autocovariate term
Abund.ac.rf=readRDS("MinnEWMresults/Abund.autocov.rf.rds")
Abund.ac.rf

abun.autocov.vimp.rf = varImpPlot(Abund.ac.rf)
abun.autocov.vimp.rf.df=as.data.frame(abun.autocov.vimp.rf)
abun.autocov.vimp.rf.df$variable=varimp.rf.prsabs.autocov$variables
p.vimp.abun.autocov.rf=ggplot(data=abun.autocov.vimp.rf.df, aes(x=reorder(variable,`%IncMSE`), y=`%IncMSE`)) +
geom_bar(stat="identity", fill="white", color="black", position=position_dodge())+
ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim=c(0,60))
p.vimp.abun.autocov.rf

png("MinnEWMfigures/SDM.Abund.AutoCov.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.abun.autocov.rf + theme_bw(base_size = 22)
dev.off()

## (e) Autocovariate response plots for all abundance sdms
abun.autologistic.sdm=read.sdm("MinnEWMresults/abun.ac.SDM.sdm")

rf.rcurve.abun.ac=rcurve(abun.autologistic.sdm, id=c(121:180), n="abund.ac", main=" ", xlab="Inverse distance", ylab="Occurrence frequency")
rf.rcurve.abun.ac.df=as.data.frame(rf.rcurve.abun.ac$data)
Autocov.Abund.resp_plot=ggplot(rf.rcurve.abun.ac.df, aes(x=Value, y=Response))+
geom_line(lwd=1)+
geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
xlab("Inverse distance")+ylab("Occurrence frequency")+coord_cartesian(ylim = c(0.0,0.25))

png("MinnEWMfigures/Abund.Autocov.respcurv.png",units="in", width=5, height=4, res=900)
Autocov.Abund.resp_plot+theme_bw(base_size = 22)
dev.off()

## (f) Growing degree days response plots for all abundance sdms
rf.rcurve.abun.gdd.ac=rcurve(abun.autologistic.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                             ylab="Occurrence frequency")
rf.rcurve.abun.gdd.ac.df=as.data.frame(rf.rcurve.abun.gdd.ac$data)

GDD.Abund.ac.resp_plot=ggplot(rf.rcurve.abun.gdd.ac.df, aes(x=Value, y=Response))+
geom_line(lwd=1)+
geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
xlab("Growing degree days")+ylab("Occurrence frequency")+coord_cartesian(ylim = c(0.0,0.25))
GDD.Abund.ac.resp_plot

png("MinnEWMfigures/Abund.Autocov.GDD.respcurv.png",units="in", width=5, height=4, res=900)
GDD.Abund.ac.resp_plot+theme_bw(base_size = 22)
dev.off()


