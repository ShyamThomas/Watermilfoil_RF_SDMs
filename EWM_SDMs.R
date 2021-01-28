
library(sdm)
library(sf)
library(randomForest)
library(tidyverse)

setwd("~/UMNpostdoc/ProjectEWM/MinnEWM")

################# SDMs for EWM presence-ABSENCE data
EWM.infes_relfrq.selpreds.prsabs=read_csv("MinnEWMdata/EWM.infes_relfrq.selpreds.prsabs.csv")
EWM.infes_relfrq.selpreds.prsabs
### Lets include connectivity variables: stream and road density
LakeIndex.ConnData = read_csv("MinnEWMdata/LakeConnectivityData.Reduced.csv")
LakeIndex.ConnData
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

### Adapt the data frame to sdm data object
SDM.data=sdmData(EWMSTATUS_corrRelFrq~.+coords(LON+LAT),train=sdm.data.df, crs=st_crs(Minn.sf))
SDM.data
plot(SDM.data)

### Run the SDM with 5 fold cross validation with 10  replicates
prsabs.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.data,methods=c('glm','gam','rf','rbf','maxlike'),
replication=c('cv','boot'), cv.fold=5,n=10)
prsabs.sdm
write.sdm(prsabs.sdm, "MinnEWMresults/prsabs.SDM", overwrite = TRUE)

###Now a quasi-indpendent evaluation; by spliting the dataset into training and test data
##A split function
splitTable = function(df, prob) {
  set.seed(077)
  variant = sample(seq(1, 0), size = nrow(df), replace = TRUE, prob = c(prob, 1 - prob))
  res = split(df, variant)
  return(res)
}

SDM.prsabs.splitdata=splitTable(sdm.data.df,0.7)
sdm.prsabs.traindata=SDM.prsabs.splitdata$'1'
dim(sdm.prsabs.traindata)
head(sdm.prsabs.traindata)
sdm.prsabs.testdata=SDM.prsabs.splitdata$'0'
dim(sdm.prsabs.testdata)
head(sdm.prsabs.testdata)

SDM.prsabs.splitdata=sdmData(EWMSTATUS_corrRelFrq~.+coords(LON+LAT),train=sdm.prsabs.traindata,
                                test= sdm.prsabs.testdata, crs=32615)
SDM.prsabs.splitdata
plot(SDM.prsabs.splitdata)

split.prsabs.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.prsabs.splitdata,methods=c('glm','gam','rf','maxlike'))
split.prsabs.sdm
write.sdm(split.prsabs.sdm, "MinnEWMresults/split.prsabs.SDM.sdm")




### Extract results of interest: variable importance, response curve, evaluation stats
rf.varimp=getVarImp(prsabs.sdm, method='rf')
plot(rf.varimp)

rf.resp=getResponseCurve(prsabs.sdm, id=c(121:180))
plot(rf.resp)
rf.rcurve.prsabs.gdd=rcurve(prsabs.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", 
                            xlab="Growing degree days", ylab="Invasion risk")
plot(rf.rcurve.prsabs.gdd)

rf.eval.prsabssdm=getEvaluation(prsabs.sdm, w=121:180, wtest="test.dep", 
                                stat = c("sensitivity", "specificity", "TSS", "AUC", "Kappa"))
rf.eval.prsabssdm


#############################################################################################################################################
############### SDMs for EWM presence-RANDOM pseudo absence data
EWM.infes_relfrq.selpreds.prs.randpseuabs=read_csv("MinnEWMdata/Rand.pseuabs.sdm.csv")
EWM.infes_relfrq.selpreds.prs.randpseuabs

EWM.infes_relfrq.Conn.preds.prs.randpseuabs=left_join(EWM.infes_relfrq.selpreds.prs.randpseuabs,LakeConn, by="DOWLKNUM")
EWM.infes_relfrq.Conn.preds.prs.randpseuabs
sdm.randpseuabs.data=EWM.infes_relfrq.Conn.preds.prs.randpseuabs[,-c(1:3,6)]
sdm.randpseuabs.data
sdm.randpseuabs.df=as.data.frame(sdm.randpseuabs.data)
head(sdm.randpseuabs.df)

sdm.randpseuabs.data=sdmData(EWMSTATUS_corrRelFrq~.+coords(LON+LAT),train=sdm.randpseuabs.df,
crs=st_crs(Minn.sf))
randpseuabs.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=sdm.randpseuabs.data,methods=c('glm','gam','rf','maxlike'),
replication='sub',test.percent=30,n=60, ncore=12)
randpseuabs.sdm
write.sdm(randpseuabs.sdm, "MinnEWMresults/randpseuabs.SDM", overwrite = TRUE)


### Using the split function described below
sdm.randpseuabs.split = splitTable(sdm.randpseuabs.df, 0.7)
sdm.randpseuabs.traindata=sdm.randpseuabs.split$'1'
head(sdm.randpseuabs.traindata)
dim(sdm.randpseuabs.traindata)
sdm.randpseuabs.testdata=sdm.randpseuabs.split$'0'
head(sdm.randpseuabs.testdata)
dim(sdm.randpseuabs.testdata)

SDM.randpseuabs.split.data=sdmData(EWMSTATUS_corrRelFrq~.+coords(LON+LAT),train=sdm.randpseuabs.traindata, test=sdm.randpseuabs.testdata,
                                  crs=32615)
SDM.randpseuabs.split.data
plot(SDM.randpseuabs.split.data)

split.randpseuabs.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.randpseuabs.split.data,methods=c('glm','gam','rf','maxlike'))
split.randpseuabs.sdm
write.sdm(split.randpseuabs.sdm, "MinnEWMresults/split.randpseuabs.SDM.sdm")

write.sdm(prs.randpseuabs.sdm, "MinnEWMresults/prs.randpseuabs.SDM")

#############################################################################################################################################
############### SDMs for EWM presence and DISTANT UNSUITABLE RANDOM pseudo-absence data

EWM.infes_relfrq.selpreds.prs.outrand.pseuabs=read_csv("MinnEWMdata/OutRand.pseuabs.sdm.csv")
EWM.infes_relfrq.selpreds.prs.outrand.pseuabs

EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs=left_join(EWM.infes_relfrq.selpreds.prs.outrand.pseuabs,LakeConn, by="DOWLKNUM")
EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs
EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs=EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs%>%
na.omit()
names(EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs)
sdm.outrandpseuabs.data=EWM.infes_relfrq.Conn.preds.prs.outrand.pseuabs[,-c(1,4,15,16)]
sdm.outrandpseuabs.data
sdm.outrand.pseuabs.df=as.data.frame(sdm.outrandpseuabs.data)
head(sdm.outrand.pseuabs.df)

sdm.outrandpseuabs.data=sdmData(EWMSTATUS_corrRelFrq~.+coords(LON+LAT),train=sdm.outrand.pseuabs.df,
crs=st_crs(Minn.sf))
sdm.outrandpseuabs.data
plot(sdm.outrandpseuabs.data)
outrandpseuabs.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=sdm.outrandpseuabs.data,methods=c('glm','gam','rf','maxlike'),
replication='sub',test.percent=30,n=60, ncore=12)
write.sdm(outrandpseuabs.sdm, "MinnEWMresults/outrandpseuabs.SDM", overwrite = TRUE)


### Extract 70 percent of the data for training and use presence absence data as test
### Make split function to extract 70 percent data
splitTable = function(df, prob) {
set.seed(077)
variant = sample(seq(1, 0), size = nrow(df), replace = TRUE, prob = c(prob, 1 - prob))
res = split(df, variant)
return(res)
}
 ### Use the above defined function to split the dataframe
prs.outrandpseuabs.sdm.split=splitTable(sdm.outrand.pseuabs.df, 0.7)
prs.outrandpseuabs.traindata=prs.outrandpseuabs.sdm.split$'1'
head(prs.outrandpseuabs.traindata)
dim(prs.outrandpseuabs.traindata)
prs.outrandpseuabs.testdata=prs.outrandpseuabs.sdm.split$'0'
head(prs.outrandpseuabs.testdata)
dim(prs.outrandpseuabs.testdata)

SDM.outrandpseuabs.splitdata=sdmData(EWMSTATUS_corrRelFrq~.+coords(LON+LAT),train=prs.outrandpseuabs.traindata, 
                                      test=prs.outrandpseuabs.testdata , crs=32615)
SDM.outrandpseuabs.splitdata
plot(SDM.outrandpseuabs.splitdata)
split.outrandpseuabs.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=SDM.outrandpseuabs.splitdata,methods=c('glm','gam','rf','maxlike'))
split.outrandpseuabs.sdm
write.sdm(split.outrandpseuabs.sdm, "MinnEWMresults/split.outrandpseuabs.SDM.sdm", overwrite = TRUE)

rf.resp.outrand.pseuabs=getResponseCurve(prs.outrandpseuabs.sdm, id=c(121:180))
plot(rf.resp.outrand.pseuabs)
rf.varimp.outrand.pseuabs=getVarImp(prs.outrandpseuabs.sdm, method='rf')
plot(rf.varimp.outrand.pseuabs)

#############################################################################################################################################
############### SDMs for EWM presence and NEAREST RANDOM pseudo-absence data
EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs=read_csv("MinnEWMdata/NearRand.pseuabs.sdm.csv")
EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs

EWM.infes_relfrq.Conn.preds.prs.nearrand.pseuabs=left_join(EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs,LakeConn, by="DOWLKNUM")
EWM.infes_relfrq.Conn.preds.prs.nearrand.pseuabs
names(EWM.infes_relfrq.Conn.preds.prs.nearrand.pseuabs)

sdm.nearrandpseuabs.data=EWM.infes_relfrq.Conn.preds.prs.nearrand.pseuabs[,-c(1,4,15,16)]
sdm.nearrand.pseuabs.df=as.data.frame(sdm.nearrandpseuabs.data)
head(sdm.nearrand.pseuabs.df)
dim(sdm.nearrand.pseuabs.df)

### Using the split function again
prs.nearrandpseuabs.sdm.split=splitTable(sdm.nearrand.pseuabs.df, 0.7)
prs.nearrandpseuabs.sdm.traindata=prs.nearrandpseuabs.sdm.split$'1'
head(prs.nearrandpseuabs.sdm.traindata)
dim(prs.nearrandpseuabs.sdm.traindata)
prs.nearrandpseuabs.sdm.testdata=prs.nearrandpseuabs.sdm.split$'0'
head(prs.nearrandpseuabs.sdm.testdata)
dim(prs.nearrandpseuabs.sdm.testdata)

sdm.nearrand.pseuabs.splitdata=sdmData(EWMSTATUS_corrRelFrq~.+coords(LON+LAT),train=prs.nearrandpseuabs.sdm.traindata, 
                                       test=prs.nearrandpseuabs.sdm.testdata , crs=32615)
sdm.nearrand.pseuabs.splitdata
plot(sdm.nearrand.pseuabs.splitdata)
split.nearrandpseuabs.sdm = sdm(EWMSTATUS_corrRelFrq~.,data=sdm.nearrand.pseuabs.splitdata,methods=c('glm','gam','rf','rbf','maxlike'))


write.sdm(prs.nearrandpseuabs.sdm, "MinnEWMresults/prs.nearrandpseuabs.SDM")

#############################################################################################################################################
##################################################################################################################################################
############### SDMs for EWM abundance data: OCCURRENCE FRQUENCY
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
dplyr::select(-c(avg_totalalkalinity, avg_nitrogen,  DOWLKNUM, UTMX, UTMY))%>%
na.omit()
EWM.relfrq.Conn.preds.data.selvars ### Data with EWM relative frequency measure of abundance
SDM.relfrq.data.sf=st_as_sf(EWM.relfrq.Conn.preds.data.selvars,coords=c("LON","LAT"),crs=st_crs(Minn.sf))
SDM.relfrq.data.sf

#### Load the data in SDM data format
SDM.relfrq.data=sdmData(MYS_relfrq~.+coords(LON+LAT),train=as.data.frame(EWM.relfrq.Conn.preds.data.selvars), crs=st_crs(Minn.sf))
SDM.relfrq.data
abun.sdm = sdm(MYS_relfrq~.,data=SDM.relfrq.data,methods=c('glm','gam','rf','maxlike'),
               replication='sub', test.percent=30, n=60, parallelSettings=list(ncore=12))
abun.sdm
write.sdm(abun.sdm, "MinnEWMresults/abun.SDM.sdm", overwrite = TRUE)

### A random forest model done seperately
EWM.abun.data.rf=EWM.relfrq.Conn.preds.data.selvars[,-c(1,2)]
EWM.abun.data.rf
abun.sdm.rf=randomForest(MYS_relfrq ~ ., data=EWM.abun.data.rf, ntree=500, keep.forest=TRUE,
importance=TRUE)



############################################################################################################################################
############################################################################################################################################
library(reshape2)
library(ggplot2)
##### Plotting interesting results from all SDM runs above

### [1] Plotting evaluation metrics
rf.eval.prsabssdm.split=getEvaluation(prsabs.sdm, w=121:180, wtest="test.dep", stat = c("sensitivity", "specificity", "TSS", "AUC", "Kappa"))
class(rf.eval.prsabssdm)
rf.eval.prs.randpseuabs.sdm.split=getEvaluation(prs.randpseuabs.sdm.split.pp, w=121:180, wtest="test.indep", stat = c("sensitivity", "specificity", "TSS", "AUC", "Kappa"))
rf.eval.prs.outrandpseuabs.sdm.split=getEvaluation(prs.outrandpseuabs.sdm.split.pp, w=121:180, wtest="test.indep", stat = c("sensitivity", "specificity", "TSS", "AUC", "Kappa"))
rf.eval.prs.nearrandpseuabs.sdm.split=getEvaluation(prs.nearrandpseuabs.sdm.split.pp, w=121:180, wtest="test.indep", stat = c("sensitivity", "specificity", "TSS", "AUC", "Kappa"))

rf.eval.prsabssdm.split$AbsenceType=rep("Absence",60)
head(rf.eval.prsabssdm.split)
rf.eval.prs.randpseuabs.sdm.split$AbsenceType=rep("PseudoAbs_Rand", 60)
head(rf.eval.prs.randpseuabs.sdm.split)
rf.eval.prs.outrandpseuabs.sdm.split$AbsenceType=rep("PseudoAbs_Dist", 60)
head(rf.eval.prs.outrandpseuabs.sdm.split)
rf.eval.prs.nearrandpseuabs.sdm.split$AbsenceType=rep("PseudoAbs_Near", 60)
head(rf.eval.prs.nearrandpseuabs.sdm.split)

AllAbsSDM.Evals.Split=rbind(rf.eval.prsabssdm.split,rf.eval.prs.randpseuabs.sdm.split,rf.eval.prs.outrandpseuabs.sdm.split,rf.eval.prs.nearrandpseuabs.sdm.split)
tail(AllAbsSDM.Evals.Split)

AllAbsSDM.Evals.Split.melt= melt(AllAbsSDM.Evals.Split,id.vars='AbsenceType', measure.vars=c('AUC','sensitivity','specificity','TSS','Kappa'))
AllAbsSDM.Evals.Split.melt= melt(AllAbsSDM.Evals.Split,id.vars='AbsenceType', measure.vars=c('AUC','TSS','Kappa'))
p <- ggplot(AllAbsSDM.Evals.Split.melt) +
geom_boxplot(aes(x=AbsenceType, y=value, fill=variable))
p

rf.eval.abun.sdm=getEvaluation(abun.sdm, w=121:180, wtest="test.dep", stat = "cor")
rf.eval.abun.sdm
rf.eval.abun.sdm$AbsenceType=rep("Abundance", 60)
head(rf.eval.abun.sdm)
rf.eval.abun.sdm$variable=rep("Corr", 60)
names(rf.eval.abun.sdm)[3]="AbsenceType"
names(rf.eval.abun.sdm)[2]="value"
head(rf.eval.abun.sdm)
rf.eval.abun.sdm2=rf.eval.abun.sdm[c('AbsenceType','variable','value')]
head(rf.eval.abun.sdm2)

AllAbsSDM.Evals.Split.melt2=rbind(AllAbsSDM.Evals.Split.melt,rf.eval.abun.sdm2)
AllAbsSDM.Evals.Split.melt2$AbsenceType = factor(AllAbsSDM.Evals.Split.melt2$AbsenceType , 
                                   levels=c("Absence", "PseudoAbs_Rand", "PseudoAbs_Dist", "PseudoAbs_Near", "Abundance"))
p <- ggplot(AllAbsSDM.Evals.Split.melt2) +
geom_boxplot(aes(x=AbsenceType, y=value, fill=variable))

p + theme(legend.title = element_blank())+ylab(" ")+xlab(" ")+scale_fill_grey(start = 0.33, end=0.99)+
                          geom_vline(xintercept = 4.5, linetype=2)

png("MinnEWMfigures/SDM.Evals.SplitModels.png",units="in", width=8, height=4, res=900)
p + theme(legend.title = element_blank())+ylab(" ")+xlab(" ")+scale_fill_grey(start = 0.33, end=0.99)+
  geom_vline(xintercept = 4.5, linetype=2)+coord_cartesian(ylim=c(0.0,1.0))
dev.off()

#### [2] Relative variable importance
## Start with surveyed absence and occurrence frequency models

rf.varimp=getVarImp(prsabs.sdm, method='rf')
varimp.rf.abs=rf.varimp@varImportanceMean$AUCtest
varimp.rf.abs$ModelType=rep("Absence",11)

p.vimp.abs=ggplot(data=varimp.rf.abs, aes(x=reorder(variables,AUCtest*100) , y=AUCtest*100)) +
geom_bar(stat="identity", fill="dark gray", color="black", position=position_dodge())+
  ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60))  

png("MinnEWMfigures/SDM.PrsAbs.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.abs
dev.off()

abun.vimp.rf <- varImpPlot(abun.sdm.rf)
abun.vimp.rf.df=as.data.frame(abun.vimp.rf)
abun.vimp.rf.df$variable=varimp.rf.abs$variables


p.vimp.abun.rf=ggplot(data=abun.vimp.rf.df, aes(x=reorder(variable,`%IncMSE`), y=`%IncMSE`)) +
geom_bar(stat="identity", fill="white", color="black", position=position_dodge())+
  ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim=c(0,60))

png("MinnEWMfigures/SDM.Abun.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.abun.rf
dev.off()


## Repeat for all pseudo-absence models
rf.varimp.rand.pseuabs=getVarImp(prs.randpseuabs.sdm, method='rf')
varimp.rf.randpseuabs=rf.varimp.rand.pseuabs@varImportanceMean$AUCtest
varimp.rf.randpseuabs$ModelType=rep("PseuAbs_Rand",11)

p.vimp.randabs=ggplot(data=varimp.rf.randpseuabs, aes(x=reorder(variables,AUCtest*100), y=AUCtest*100)) +
geom_bar(stat="identity", fill="light gray", color="black", position=position_dodge())+
  ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60))

png("MinnEWMfigures/SDM.PrsRandAbs.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.randabs
dev.off()

rf.varimp.outrand.pseuabs=getVarImp(prs.outrandpseuabs.sdm, method='rf')
varimp.rf.outrandpseuabs=rf.varimp.outrand.pseuabs@varImportanceMean$AUCtest
varimp.rf.outrandpseuabs$ModelType=rep("PseuAbs_Dist",11)

p.vimp.distrandabs=ggplot(data=varimp.rf.outrandpseuabs, aes(x=reorder(variables,AUCtest*100), y=AUCtest*100)) +
geom_bar(stat="identity", fill="light gray", color="black", position=position_dodge())+
  ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60))
p.vimp.distrandabs

png("MinnEWMfigures/SDM.PrsDistRandAbs.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.distrandabs
dev.off()
p.vimp.dist

rf.varimp.nearrand.pseuabs=getVarImp(prs.nearrandpseuabs.sdm, method='rf')
varimp.rf.nearrandpseuabs=rf.varimp.nearrand.pseuabs@varImportanceMean$AUCtest
varimp.rf.nearrandpseuabs$ModelType=rep("PseuAbs_Near",11)
varimp.rf.nearrandpseuabs

p.vimp.nearrandabs=ggplot(data=varimp.rf.nearrandpseuabs, aes(x=reorder(variables,AUCtest*100), y=AUCtest*100))+
  geom_bar(stat="identity", fill="light gray", color="black", position=position_dodge())+
  ylab("Relative variable importance")+xlab(" ")+coord_flip(ylim = c(0,60))

png("MinnEWMfigures/SDM.PrsNearRandAbs.VarImp.png",units="in", width=5, height=4, res=900)
p.vimp.nearrandabs
dev.off()

#### [3] Plotting response curves
## Only 3 most important variables were chosen: GDD, road density, and max depth
## (1) Growing degree days response curves
rf.rcurve.prsabs.gdd=rcurve(prsabs.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="GDD", ylab="Invasion risk")
rf.rcurve.randpseuabs.gdd=rcurve(prs.randpseuabs.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                                 ylab="Invasion risk")
rf.rcurve.outrandpseuabs.gdd=rcurve(prs.outrandpseuabs.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                                  ylab="Invasion risk")
rf.rcurve.nearrandpseuabs.gdd=rcurve(prs.nearrandpseuabs.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days", 
                                     ylab="Invasion risk")


rf.rcurve.prsabs.gdd$data$Model=rep("Absence", 100)
rf.rcurve.randpseuabs.gdd$data$Model=rep("Pseudoabs_Rand", 100)
rf.rcurve.outrandpseuabs.gdd$data$Model=rep("Pseudoabs_Dist", 100)
rf.rcurve.nearrandpseuabs.gdd$data$Model=rep("Pseudoabs_Near", 100)

GDD.allmodels.rcurves=rbind(rf.rcurve.prsabs.gdd$data,rf.rcurve.randpseuabs.gdd$data,rf.rcurve.outrandpseuabs.gdd$data,
                            rf.rcurve.nearrandpseuabs.gdd$data)
head(GDD.allmodels.rcurves)

GDD.invrisk.plot=ggplot(GDD.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
geom_line(lwd=1)+
geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.title = element_blank())+
xlab("Growing degree days")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))

## (2) Road density plots
rf.rcurve.prsabs.road=rcurve(prsabs.sdm, id=c(121:180), n="roaddensity_density_mperha", main=" ", xlab="Road density (mpha)", 
                            ylab="Invasion risk")
rf.rcurve.outrandpseuabs.road=rcurve(prs.outrandpseuabs.sdm, id=c(121:180), n="roaddensity_density_mperha", main=" ",
                                     xlab="Road density (mpha)", ylab="Invasion risk")
rf.rcurve.randpseuabs.road=rcurve(prs.randpseuabs.sdm, id=c(121:180), n="roaddensity_density_mperha", main=" ", 
                                  xlab="Road density (mpha)", ylab="Invasion risk")
rf.rcurve.nearrandpseuabs.road=rcurve(prs.nearrandpseuabs.sdm, id=c(121:180), n="roaddensity_density_mperha", main=" ", 
                                      xlab="Road density (mpha)", ylab="Invasion risk")


rf.rcurve.prsabs.road$data$Model=rep("Absence", 100)
rf.rcurve.randpseuabs.road$data$Model=rep("Pseudoabs_Rand", 100)
rf.rcurve.outrandpseuabs.road$data$Model=rep("Pseudoabs_Dist", 100)
rf.rcurve.nearrandpseuabs.road$data$Model=rep("Pseudoabs_Near", 100)
Road.allmodels.rcurves=rbind(rf.rcurve.prsabs.road$data,rf.rcurve.randpseuabs.road$data,rf.rcurve.outrandpseuabs.road$data,
rf.rcurve.nearrandpseuabs.road$data)


Road.invrisk.plot=ggplot(Road.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
geom_line(lwd=1)+
geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.position = "NA")+
xlab("Road density (mpha)")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))

png("MinnEWMfigures/PrsAbsSDM.Road.respcurv.png",units="in", width=4.5, height=4, res=900)
Road.invrisk.plot
dev.off()

## (3) Lake depth plots
rf.rcurve.prsabs.depth=rcurve(prsabs.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Lake depth (m)",
ylab="Invasion risk")
rf.rcurve.outrandpseuabs.depth=rcurve(prs.outrandpseuabs.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Lake depth (m)",
ylab="Invasion risk")
rf.rcurve.randpseuabs.depth=rcurve(prs.randpseuabs.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Lake depth (m)",
ylab="Invasion risk")
rf.rcurve.nearrandpseuabs.depth=rcurve(prs.nearrandpseuabs.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Lake depth (m)",
ylab="Invasion risk")

rf.rcurve.prsabs.depth$data$Model=rep("Absence", 100)
rf.rcurve.randpseuabs.depth$data$Model=rep("Pseudoabs_Rand", 100)
rf.rcurve.outrandpseuabs.depth$data$Model=rep("Pseudoabs_Dist", 100)
rf.rcurve.nearrandpseuabs.depth$data$Model=rep("Pseudoabs_Near", 100)
Depth.allmodels.rcurves=rbind(rf.rcurve.prsabs.depth$data,rf.rcurve.randpseuabs.depth$data,rf.rcurve.outrandpseuabs.depth$data,
rf.rcurve.nearrandpseuabs.depth$data)
head(Depth.allmodels.rcurves)

Depth.invrisk.plot=ggplot(Depth.allmodels.rcurves, aes(x=Value, y=Response, col=Model))+
geom_line(lwd=1)+
geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.33, linetype=0)+
scale_color_manual(values=c("black","red","blue","orange"))+theme(legend.position = "NA")+
xlab("Lake depth (m)")+ylab("Invasion risk")+coord_cartesian(ylim = c(0.0,1.0))

png("MinnEWMfigures/PrsAbsSDM.Depth.respcurv.png",units="in", width=4.5, height=4, res=900)
Depth.invrisk.plot
dev.off()

### Plot 3 most important variables for abundance models
rf.rcurve.abun.gdd=rcurve(abun.sdm, id=c(121:180), n="mean.gdd_wtr_10c", main=" ", xlab="Growing degree days",
ylab="Occurrence frequency")
png("MinnEWMfigures/AbunSDM.gdd.respcurv.png",units="in", width=5, height=4, res=900)
plot(rf.rcurve.abun.gdd)+coord_cartesian(ylim=c(0.0,0.2))
dev.off()


rf.rcurve.abun.secchi=rcurve(abun.sdm, id=c(121:180), n="avg_secchi", main=" ", xlab="Secchi depth",
ylab="Occurrence frequency")
png("MinnEWMfigures/AbunSDM.secchi.respcurv.png",units="in", width=5, height=4, res=900)
plot(rf.rcurve.abun.secchi)+coord_cartesian(ylim=c(0.0,0.2))+xlab("Secchi depth (m)")
dev.off()


rf.rcurve.abun.depth=rcurve(abun.sdm, id=c(121:180), n="max_depth", main=" ", xlab="Maximum depth",
ylab="Occurrence frequency")
png("MinnEWMfigures/AbunSDM.depth.respcurv.png",units="in", width=5, height=4, res=900)
plot(rf.rcurve.abun.depth)+coord_cartesian(ylim=c(0.0,0.2))+xlab("Lake depth (m)")
dev.off()

rf.rcurve.abun.road=rcurve(abun.sdm, id=c(121:180), n="roaddensity_density_mperha", main=" ", xlab="Road density (mpha)",
                           ylab="Invasion risk")


############## Visualizing interactions between key predictors using pdp package
library(pdp)
library(vip)

prsabs.rf=randomForest(as.factor(EWMSTATUS_corrRelFrq) ~., data=sdm.data.rf, importance=TRUE, na.action = na.omit)
prsabs.rf
vip(prsabs.rf, bar = FALSE, horizontal = TRUE, size = 1.5) ### Variable importance plot

depth.gdd.partial=partial(prsabs.rf, pred.var = c("mean.gdd_wtr_10c", "max_depth"), prob=TRUE, which.class=2L, chull = TRUE, 
                                                    progress = "text")
plotPartial(depth.gdd.partial)

gdd.road.partial=partial(prsabs.rf, pred.var = c("mean.gdd_wtr_10c", "roaddensity_density_mperha"), prob=TRUE, which.class=2L,
                         chull = TRUE, progress = "text")
plotPartial(gdd.road.partial)

road.depth.partial=partial(prsabs.rf, pred.var = c("roaddensity_density_mperha", "max_depth"), prob=TRUE, which.class=2L,chull = TRUE, progress ="text")
plotPartial(road.depth.partial, xlab="Road density", ylab="Lake depth", legend.title="Prob", contour = TRUE)


############################################################################################################################################
############################################################################################################################################
####### Does predicted suitability from presence-absence model predict abundance? The Habitat Suitability-Abundance Relationship

sdm.data.pred.df=sdm.data.df[,-c(1:3)] ### from line 22
head(sdm.data.pred.df)
predict.sdm.rf=predict(prsabs.sdm, newdata=sdm.data.pred.df, method="rf", mean=TRUE) ### predicted values from the presence-absence SDM above
head(predict.sdm.rf)
predict.sdm.rf.df=as.data.frame(predict.sdm.rf) ### All predicted values in a dataframe

EWM.relfrq.index=EWM.relfrq.Conn.preds.data[,c(1,4:6)] ## from line 130 above in the abundance modeling section
EWM.infes_relfrq.Conn.preds.prsabs
EWM.infes_relfrq.Conn.preds.prsabs.naomit=EWM.infes_relfrq.Conn.preds.prsabs%>% na.omit()
EWM.infes_relfrq.Conn.preds.prsabs.naomit
sdm.data.DOW.latlon=EWM.infes_relfrq.Conn.preds.prsabs.naomit[,c(1,4,5,7)] ## from line 19 in the above presence absence SDM section
sdm.data.DOW.latlon
sdm.data.DOW.latlon$predvalues=predict.sdm.rf.df$`sp_1-m_rf-re_subs`

sdm.suitability_abundance.data=left_join(sdm.data.DOW.latlon,EWM.relfrq.index, by="DOWLKNUM")
sdm.suitability_abundance.data
sdm.suitability_abundance.naomit=sdm.suitability_abundance.data%>%
na.omit()
sdm.suitability_abundance.naomit

png("MinnEWMfigures/Suitaibility_AbundancePlot.png",units="in", width=5, height=4, res=900)

ggplot(sdm.suitability_abundance.naomit, aes(predvalues,MYS_relfrq))+
geom_point()+
geom_quantile(quantiles=c( 0.3,0.6, 0.9),formula=y~poly(x,3), colour="red")+
xlab("Predicted suitability")+ylab("Occurrence frequency")+
coord_cartesian(ylim=c(0.0,1.0))
dev.off()

#### Repeat the above steps for the PSEUDO-ABSENCE based model predictions
predict.randpseuabs.sdm.rf=predict(prs.randabs.sdm.split.pp, newdata=sdm.data.pred.df, method="rf", mean=TRUE)
tail(predict.randpseuabs.sdm.rf)
predict.randpseuabs.sdm.rf.df=as.data.frame(predict.randpseuabs.sdm.rf)
sdm.data.DOW.latlon$randpeseuabs_predvalues=predict.randpseuabs.sdm.rf.df$`sp_1-m_rf-re_subs`
sdm.data.DOW.latlon

predict.outrand.pseuabs.sdm.rf=predict(outrandabs.sdm, newdata=sdm.data.pred.df, method="rf", mean=TRUE)
tail(predict.outrand.pseuabs.sdm.rf)
predict.outrand.pseuabs.sdm.rf.df=as.data.frame(predict.outrand.pseuabs.sdm.rf)
sdm.data.DOW.latlon$outrandpeseuabs_predvalues=predict.outrand.pseuabs.sdm.rf.df$`sp_1-m_rf-re_subs`
sdm.data.DOW.latlon

predict.nearrand.pseuabs.sdm.rf=predict(nearrandabs.sdm, newdata=sdm.data.pred.df, method="rf", mean=TRUE)
tail(predict.nearrand.pseuabs.sdm.rf)
predict.nearrand.pseuabs.sdm.rf.df=as.data.frame(predict.nearrand.pseuabs.sdm.rf)
sdm.data.DOW.latlon$nearrandpeseuabs_predvalues=predict.nearrand.pseuabs.sdm.rf.df$`sp_1-m_rf-re_subs`
sdm.data.DOW.latlon

sdm.suitability_abundance.randpseuabs.data=left_join(sdm.data.DOW.latlon,EWM.relfrq.index, by="DOWLKNUM")
sdm.suitability_abundance.randpseuabs.naomit=sdm.suitability_abundance.randpseuabs.data%>%
na.omit()
sdm.suitability_abundance.randpseuabs.naomit

randabs_abundance.plot=ggplot(sdm.suitability_abundance.naomit, aes(randpeseuabs_predvalues,MYS_relfrq))+
geom_point()+
geom_quantile(quantiles=c( 0.3,0.6, 0.9),formula=y~poly(x,3), colour="red")+
xlab("Predicted suitability (random pseudo-absence model)")+ylab("Occurrence frequency")+
coord_cartesian(ylim=c(0.0,1.0))
### After repeating the above steps for all other pseudo-absence models
grid.arrange(prsabs_abundance.plot, randabs_abundance.plot,outrandabs_abundance.plot, nearrandabs_abundance.plot, ncol=2)

######################################################################################################################################
#####################################################################################################################################
