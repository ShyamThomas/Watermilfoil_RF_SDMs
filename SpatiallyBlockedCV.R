### Spatially blocked cross-validation
### Continues from EWM_SDMs and EWN_AutocovSDMs

library(precrec)
library(maptools)
library(spatstat)
library(blockCV)

### Load the desired species distribution dataset: 
######## 1) Presence-Absence with Autovariate dataset is shown here because UTMs are needed
prsabs.lakes.utm=SpatialPoints(cbind(sdm.ac.data.df$UTMX,sdm.ac.data.df$UTMY), proj4string=CRS("+proj=utm +zone=15"))
prsabs.lakes.ppp=as.ppp.SpatialPoints(prsabs.lakes.utm)
prsabs.lakes.ppp
plot(prsabs.lakes.ppp)
prsabs.lakes.pcf=envelope(prsabs.lakes.ppp, pcf, nsim = 99, correction = "all", verbose = F)
prsabs.lakes.pcf
plot(prsabs.lakes.pcf, ylim=c(0,10))

sdm.data.utm.sf
sdm.data.utm.sf=sdm.data.utm.sf %>%
na.omit()

trial.blocks = spatialBlock(speciesData = sdm.data.utm.sf, # sf or SpatialPoints
species = "EWMSTATUS_corrRelFrq", # the response column (binomial or multi-class)
theRange = 100000, # size of the blocks in meters
k = 5, # number of folds
selection = "random",
iteration = 100, # find evenly dispersed folds
biomod2Format = FALSE)

Minn.sf=read_sf(dsn="/Users/thom7552/UMNpostdoc/ProjectEWM/MinnEWM/MinnGISlayers", layer="Minn.map")
plot(Minn.sf$geometry)
Minn.sf
st_crs(Minn.sf)

png("MinnEWMfigures/BlockCV_random.png",units="in", width=4.5, height=5, res=900)
trial.blocks$plots+theme(panel.grid.major = element_blank())+
geom_sf(data = Minn.sf, fill=NA, colour="gray", lwd=1)+
geom_sf(data = sdm.data.utm.sf, alpha = 0.5, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+theme(legend.title=element_blank())
dev.off()

folds=trial.blocks$folds

### Now run the random forest SDMs using the the SDM presence-absence dataset
sdm.data.utm.sf=sdm.data.utm.sf %>%
na.omit()
sdm.data.utm.sf
sdm.testdata=st_drop_geometry(sdm.data.utm.sf)%>%
na.omit()
dim(sdm.testdata)
sdm.testdata$pred=NA

sdm.data.utm.df2=sdm.data.utm.df[,-c(1,2)]%>%na.omit()
head(sdm.data.utm.df2)


for(k in seq_len(length(folds))){
# extracting the training and testing indices
# this way works with folds list (but not foldID)
trainSet <- unlist(folds[[k]][1]) # training set indices
testSet <- unlist(folds[[k]][2]) # testing set indices
rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., sdm.data.utm.df2[trainSet, ], ntree = 250) # model fitting on training set
sdm.testdata$pred[testSet] <- predict(rf, sdm.data.utm.df2[testSet, ], type = "prob")[,2] # predict the test set
}
precrec_obj = evalmod(scores = sdm.testdata$pred, labels = sdm.testdata$EWMSTATUS_corrRelFrq)
precrec_obj ## This should give the AUC

### To calculate TSS & Kappa; we need few more steps
sdm.pred=prediction(sdm.testdata$pred, sdm.testdata$EWMSTATUS_corrRelFrq) ## make a prediction object

library(ROCR)
Sens.model.RM = performance(sdm.pred,  measure="sens", x.measure="cutoff")
Sens.model.RM
Spec.model.RM = performance(sdm.pred,  measure="spec", x.measure="cutoff")
Spec.model.RM

both.eq <- which.min(abs(Sens.model.RM@y.values[[1]]-Spec.model.RM@y.values[[1]]))
Sens.model.RM@x.values[[1]][both.eq] ### Threshold value where sensitivity equals specificity
plot(Sens.model.RM, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.model.RM, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = 0.348, col = "black", lty = 3)

library(ModelMetrics) ## Now calculate the Kappa and TSS values
sdm.kappa=kappa(sdm.testdata$EWMSTATUS_corrRelFrq,sdm.testdata$pred, cutoff = 0.348)
sdm.sens=sensitivity(sdm.testdata$EWMSTATUS_corrRelFrq,sdm.testdata$pred, cutoff = 0.348)
sdm.sens
sdm.spec=sensitivity(sdm.testdata$EWMSTATUS_corrRelFrq,sdm.testdata$pred, cutoff = 0.348)
sdm.spec
sdm.tss=sdm.sens+sdm.spec-1

### Now run the random forest SDMs using the the SDM presence-absence with AUTOCOVARIATE dataset
head(sdm.ac.data.df)
sdm.ac.data.df2=sdm.ac.data.df[,-c(1,2)]%>%na.omit()
dim(sdm.ac.data.df2)

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., sdm.ac.data.df2[trainSet, ], ntree = 250) # model fitting on training set
  sdm.testdata$pred[testSet] <- predict(rf, sdm.ac.data.df2[testSet, ], type = "prob")[,2] # predict the test set
}

precrec_obj = evalmod(scores = sdm.testdata$pred, labels = sdm.testdata$EWMSTATUS_corrRelFrq)
precrec_obj ## This should give the AUC
### To calculate TSS & Kappa; we need few more steps
sdm.pred=prediction(sdm.testdata$pred, sdm.testdata$EWMSTATUS_corrRelFrq) ## make a prediction object
Sens.model.RM = performance(sdm.pred,  measure="sens", x.measure="cutoff")
Sens.model.RM
Spec.model.RM = performance(sdm.pred,  measure="spec", x.measure="cutoff")
Spec.model.RM
both.eq <- which.min(abs(Sens.model.RM@y.values[[1]]-Spec.model.RM@y.values[[1]]))
Sens.model.RM@x.values[[1]][both.eq] ### Threshold value where sensitivity equals specificity
plot(Sens.model.RM, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.model.RM, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = 0.41, col = "black", lty = 3)

library(ModelMetrics) ## Now calculate the Kappa and TSS values
sdm.kappa=kappa(sdm.testdata$EWMSTATUS_corrRelFrq,sdm.testdata$pred, cutoff = 0.41)
sdm.kappa
sdm.sens=sensitivity(sdm.testdata$EWMSTATUS_corrRelFrq,sdm.testdata$pred, cutoff = 0.41)
sdm.sens
sdm.spec=sensitivity(sdm.testdata$EWMSTATUS_corrRelFrq,sdm.testdata$pred, cutoff = 0.41)
sdm.spec
sdm.tss=sdm.sens+sdm.spec-1
sdm.tss
#############################################################################################################################################
####### 2) Presence-Random Pseudoabsence with Autovariate dataset is shown here because UTMs are needed
prs.randpseuabs.lakes.utm=SpatialPoints(cbind(sdm.ac.randpseuabs.data$UTMX,sdm.ac.randpseuabs.data$UTMY), 
                                        proj4string=CRS("+proj=utm +zone=15"))
prs.randpseuabs.lakes.ppp=as.ppp.SpatialPoints(prs.randpseuabs.lakes.utm)
prs.randpseuabs.lakes.ppp
plot(prs.randpseuabs.lakes.ppp)
prs.randpseuabs.lakes.pcf=envelope(prs.randpseuabs.lakes.ppp, pcf, nsim = 99, correction = "all", verbose = F)
prs.randpseuabs.lakes.pcf
plot(prs.randpseuabs.lakes.pcf, ylim=c(0,10)) ## at about 100k mts again

sdm.randpseuabs.data.utm.sf ### the presence-only dataset with randomized pseudo-absences

trial.blocks.randPO = spatialBlock(speciesData = sdm.randpseuabs.data.utm.sf, # sf or SpatialPoints
                            species = "EWMSTATUS_corrRelFrq", # the response column (binomial or multi-class)
                            theRange = 100000, # size of the blocks in meters
                            k = 5, # number of folds
                            selection = "random",
                            iteration = 100, # find evenly dispersed folds
                            biomod2Format = FALSE)
png("MinnEWMfigures/BlockCV_random_POrand.png",units="in", width=4.5, height=5, res=900)
trial.blocks.randPO$plots+theme(panel.grid.major = element_blank())+
geom_sf(data = Minn.sf, fill=NA, colour="gray", lwd=1)+
geom_sf(data = sdm.randpseuabs.data.utm.sf, alpha = 0.5, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+theme(legend.title=element_blank())
dev.off()

folds=trial.blocks.randPO$folds

### Now run the random forest SDMs using the the SDM presence-random absence dataset
sdm.randpseuabs.data.utm.sf=sdm.randpseuabs.data.utm.sf %>%
  na.omit()
sdm.randpseuabs.data.utm.sf
sdm.randpseuabs.testdata=st_drop_geometry(sdm.randpseuabs.data.utm.sf)%>%
  na.omit()
dim(sdm.randpseuabs.testdata)
sdm.randpseuabs.testdata$pred=NA
head(sdm.randpseuabs.testdata)

head(sdm.randpseuabs.df)
sdm.randpseuabs.df2=sdm.randpseuabs.df[,-c(1,2)]
head(sdm.randpseuabs.df2)

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., sdm.randpseuabs.df2[trainSet, ], ntree = 250) # model fitting on training set
  sdm.randpseuabs.testdata$pred[testSet] <- predict(rf, sdm.randpseuabs.df2[testSet, ], type = "prob")[,2] # predict the test set
}
precrec_obj.PO_rand = evalmod(scores = sdm.randpseuabs.testdata$pred, labels = sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq)

### To calculate TSS & Kappa; we need few more steps
library(ROCR)

sdm.pred.randPO=prediction(sdm.randpseuabs.testdata$pred, sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq) ## make a prediction object
Sens.model.RM = performance(sdm.pred.randPO,  measure="sens", x.measure="cutoff")
Sens.model.RM
Spec.model.RM = performance(sdm.pred.randPO,  measure="spec", x.measure="cutoff")
Spec.model.RM

both.eq <- which.min(abs(Sens.model.RM@y.values[[1]]-Spec.model.RM@y.values[[1]]))
Sens.model.RM@x.values[[1]][both.eq] ### Threshold value where sensitivity equals specificity
plot(Sens.model.RM, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.model.RM, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = 0.42, col = "black", lty = 3)

library(ModelMetrics) ## Now calculate the Kappa and TSS values
sdm.kappa=kappa(sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.randpseuabs.testdata$pred, cutoff = 0.42)
sdm.sens=sensitivity(sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.randpseuabs.testdata$pred, cutoff = 0.42)
sdm.sens
sdm.spec=sensitivity(sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.randpseuabs.testdata$pred, cutoff = 0.42)
sdm.spec
sdm.tss=sdm.sens+sdm.spec-1
sdm.tss

### Now run the random forest SDMs using PRESENCE and RANDOM PSEUDOABSENCE with AUTOCOVARIATE dataset
head(sdm.ac.randpseuabs.data)
sdm.ac.randpseuabs.data.df2=sdm.ac.randpseuabs.data[,-c(1,2)]%>%na.omit()
head(sdm.ac.randpseuabs.data.df2)
dim(sdm.ac.randpseuabs.data.df2)

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., sdm.ac.randpseuabs.data.df2[trainSet, ], ntree = 250) # model fitting on training set
  sdm.randpseuabs.testdata$pred[testSet] <- predict(rf, sdm.ac.randpseuabs.data.df2[testSet, ], type = "prob")[,2] # predict the test set
}
precrec_obj.PO_randAC = evalmod(scores = sdm.randpseuabs.testdata$pred, labels = sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq)
precrec_obj.PO_randAC

sdm.pred.randPO=prediction(sdm.randpseuabs.testdata$pred, sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq) ## make a prediction object
Sens.model.RM = performance(sdm.pred.randPO,  measure="sens", x.measure="cutoff")
Sens.model.RM
Spec.model.RM = performance(sdm.pred.randPO,  measure="spec", x.measure="cutoff")
Spec.model.RM

both.eq <- which.min(abs(Sens.model.RM@y.values[[1]]-Spec.model.RM@y.values[[1]]))
Sens.model.RM@x.values[[1]][both.eq] ### Threshold value where sensitivity equals specificity
plot(Sens.model.RM, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.model.RM, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = 0.46, col = "black", lty = 3)

sdm.kappa=kappa(sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.randpseuabs.testdata$pred, cutoff = 0.46)
sdm.kappa
sdm.sens=sensitivity(sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.randpseuabs.testdata$pred, cutoff = 0.46)
sdm.sens
sdm.spec=sensitivity(sdm.randpseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.randpseuabs.testdata$pred, cutoff = 0.46)
sdm.spec
sdm.tss=sdm.sens+sdm.spec-1
sdm.tss
#############################################################################################################################################
####### 3) Presence-Distant Pseudoabsence with Autovariate dataset is shown here because UTMs are needed
prs.outrandpseuabs.lakes.utm=SpatialPoints(cbind(sdm.ac.outrand.pseuabs.data$UTMX,sdm.ac.outrand.pseuabs.data$UTMY), 
                                        proj4string=CRS("+proj=utm +zone=15"))
prs.outrandpseuabs.lakes.ppp=as.ppp.SpatialPoints(prs.outrandpseuabs.lakes.utm)
prs.outrandpseuabs.lakes.ppp
plot(prs.outrandpseuabs.lakes.ppp)
prs.outrandpseuabs.lakes.pcf=envelope(prs.outrandpseuabs.lakes.ppp, pcf, nsim = 99, correction = "all", verbose = F)
prs.outrandpseuabs.lakes.pcf
plot(prs.outrandpseuabs.lakes.pcf, ylim=c(0,10)) ## at about 100k mts again

sdm.outrand.pseuabs.data.utm.sf
sdm.outrand.pseuabs.data.utm.sf=sdm.outrand.pseuabs.data.utm.sf%>%
na.omit()

trial.blocks.distrandPO = spatialBlock(speciesData = sdm.outrand.pseuabs.data.utm.sf, # sf or SpatialPoints
                                   species = "EWMSTATUS_corrRelFrq", # the response column (binomial or multi-class)
                                   theRange = 100000, # size of the blocks in meters
                                   k = 5, # number of folds
                                   selection = "random",
                                   iteration = 100, # find evenly dispersed folds
                                   biomod2Format = FALSE)
### Map it...
trial.blocks.distrandPO$plots+theme(panel.grid.major = element_blank())+
  geom_sf(data = Minn.sf, fill=NA, colour="gray", lwd=1)+
  geom_sf(data = sdm.outrand.pseuabs.data.utm.sf, alpha = 0.5, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+
  theme(legend.title=element_blank())

### Get the 5 folds
folds=trial.blocks.distrandPO$folds

### Now run the random forest SDMs using the the SDM presence-absence dataset
sdm.outrand.pseuabs.data.utm.sf
sdm.outrand.pseuabs.testdata=st_drop_geometry(sdm.outrand.pseuabs.data.utm.sf)%>%
  na.omit()
dim(sdm.outrand.pseuabs.testdata)
sdm.outrand.pseuabs.testdata$pred=NA
head(sdm.outrand.pseuabs.testdata)

head(sdm.outrand.pseuabs.df)
sdm.outrand.pseuabs.df2=sdm.outrand.pseuabs.df[,-c(11,12)] %>%na.omit() ### removing UTMs
head(sdm.outrand.pseuabs.df2)

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., sdm.outrand.pseuabs.df2[trainSet, ], ntree = 250) # model fitting on training set
  sdm.outrand.pseuabs.testdata$pred[testSet] <- predict(rf, sdm.outrand.pseuabs.df2[testSet, ], type = "prob")[,2] # predict the test set
}

precrec_obj.PO_distrand = evalmod(scores = sdm.outrand.pseuabs.testdata$pred, labels = sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq)

### To calculate TSS & Kappa; we need few more steps
library(ROCR)

sdm.pred.DistrandPO=prediction(sdm.outrand.pseuabs.testdata$pred, sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq) ## make a prediction object
Sens.model.RM = performance(sdm.pred.DistrandPO,  measure="sens", x.measure="cutoff")
Sens.model.RM
Spec.model.RM = performance(sdm.pred.DistrandPO,  measure="spec", x.measure="cutoff")
Spec.model.RM

both.eq <- which.min(abs(Sens.model.RM@y.values[[1]]-Spec.model.RM@y.values[[1]]))
Sens.model.RM@x.values[[1]][both.eq] ### Threshold value where sensitivity equals specificity
plot(Sens.model.RM, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.model.RM, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = 0.424, col = "black", lty = 3)

library(ModelMetrics) ## Now calculate the Kappa and TSS values
sdm.kappa=kappa(sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.outrand.pseuabs.testdata$pred, cutoff = 0.424)
sdm.kappa
sdm.sens=sensitivity(sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.outrand.pseuabs.testdata$pred, cutoff = 0.424)
sdm.sens
sdm.spec=sensitivity(sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.outrand.pseuabs.testdata$pred, cutoff = 0.424)
sdm.spec
sdm.tss=sdm.sens+sdm.spec-1
sdm.tss

### Now run the random forest SDMs using PRESENCE and DISTANT PSEUDOABSENCE with AUTOCOVARIATE dataset
head(sdm.ac.outrand.pseuabs.data)
dim(sdm.ac.outrand.pseuabs.data)

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., sdm.ac.outrand.pseuabs.df2[trainSet, ], ntree = 250) # model fitting on training set
  sdm.outrand.pseuabs.testdata$pred[testSet] <- predict(rf, sdm.ac.outrand.pseuabs.df2[testSet, ], type = "prob")[,2] # predict the test set
}
precrec_obj.PO_ACdistrand = evalmod(scores = sdm.outrand.pseuabs.testdata$pred, labels = sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq)
precrec_obj.PO_ACdistrand

sdm.pred.DistrandPO=prediction(sdm.outrand.pseuabs.testdata$pred, sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq) ## make a prediction object
Sens.model.RM = performance(sdm.pred.DistrandPO,  measure="sens", x.measure="cutoff")
Sens.model.RM
Spec.model.RM = performance(sdm.pred.DistrandPO,  measure="spec", x.measure="cutoff")
Spec.model.RM

both.eq <- which.min(abs(Sens.model.RM@y.values[[1]]-Spec.model.RM@y.values[[1]]))
Sens.model.RM@x.values[[1]][both.eq] ### Threshold value where sensitivity equals specificity
plot(Sens.model.RM, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.model.RM, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = 0.264, col = "black", lty = 3)

sdm.kappa=kappa(sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.outrand.pseuabs.testdata$pred, cutoff = 0.264)
sdm.kappa
sdm.sens=sensitivity(sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.outrand.pseuabs.testdata$pred, cutoff = 0.264)
sdm.sens
sdm.spec=sensitivity(sdm.outrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.outrand.pseuabs.testdata$pred, cutoff = 0.264)
sdm.spec
sdm.tss=sdm.sens+sdm.spec-1
sdm.tss
#############################################################################################################################################
####### 4) Presence-Proximal Pseudoabsence with Autovariate dataset is shown here because UTMs are needed
prs.nearrandpseuabs.lakes.utm=SpatialPoints(cbind(sdm.ac.nearrand.pseuabs.data$UTMX,sdm.ac.nearrand.pseuabs.data$UTMY), 
                                           proj4string=CRS("+proj=utm +zone=15"))

prs.nearrandpseuabs.lakes.ppp=as.ppp.SpatialPoints(prs.nearrandpseuabs.lakes.utm)
prs.nearrandpseuabs.lakes.ppp
plot(prs.nearrandpseuabs.lakes.ppp)
prs.nearrandpseuabs.lakes.pcf=envelope(prs.nearrandpseuabs.lakes.ppp, pcf, nsim = 99, correction = "all", verbose = F)
prs.nearrandpseuabs.lakes.pcf
plot(prs.nearrandpseuabs.lakes.pcf) ## at about 50k mts again

sdm.nearrand.pseuabs.data.utm.sf
sdm.nearrand.pseuabs.data.utm.sf%>%
  na.omit()

trial.blocks.nearrandPO = spatialBlock(speciesData = sdm.nearrand.pseuabs.data.utm.sf, # sf or SpatialPoints
                                       species = "EWMSTATUS_corrRelFrq", # the response column (binomial or multi-class)
                                       theRange = 50000, # size of the blocks in meters
                                       k = 5, # number of folds
                                       selection = "random",
                                       iteration = 100, # find evenly dispersed folds
                                       biomod2Format = FALSE)

### Map it...
trial.blocks.nearrandPO$plots+theme(panel.grid.major = element_blank())+
  geom_sf(data = Minn.sf, fill=NA, colour="gray", lwd=1)+
  geom_sf(data = sdm.nearrand.pseuabs.data.utm.sf, alpha = 0.5, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+
  theme(legend.title=element_blank())

### Get the 5 folds
folds=trial.blocks.nearrandPO$folds

### Now run the random forest SDMs using the the SDM presence-absence dataset
sdm.nearrand.pseuabs.data.utm.sf   
sdm.nearrand.pseuabs.testdata=st_drop_geometry(sdm.nearrand.pseuabs.data.utm.sf)%>%
  na.omit()
dim(sdm.nearrand.pseuabs.testdata)
sdm.nearrand.pseuabs.testdata$pred=NA
head(sdm.nearrand.pseuabs.testdata)

head(sdm.nearrand.pseuabs.df)
sdm.nearrand.pseuabs.df2=sdm.nearrand.pseuabs.df[,-c(11,12)] %>%na.omit() ### removing UTMs
head(sdm.nearrand.pseuabs.df2)
dim(sdm.nearrand.pseuabs.df2)

for(k in seq_len(length(folds))){
  # extracting the training and testing indices
  # this way works with folds list (but not foldID)
  trainSet <- unlist(folds[[k]][1]) # training set indices
  testSet <- unlist(folds[[k]][2]) # testing set indices
  rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., sdm.nearrand.pseuabs.df2[trainSet, ], ntree = 250) # model fitting on training set
  sdm.nearrand.pseuabs.testdata$pred[testSet] <- predict(rf, sdm.nearrand.pseuabs.df2[testSet, ], type = "prob")[,2] # predict the test set
}

precrec_obj.POnearrand = evalmod(scores = sdm.nearrand.pseuabs.testdata$pred, labels = sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq)

### To calculate TSS & Kappa; we need few more steps
library(ROCR)

sdm.pred.NearrandPO=prediction(sdm.nearrand.pseuabs.testdata$pred, sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq) ## make a prediction object
Sens.model.RM = performance(sdm.pred.NearrandPO,  measure="sens", x.measure="cutoff")
Sens.model.RM
Spec.model.RM = performance(sdm.pred.NearrandPO,  measure="spec", x.measure="cutoff")
Spec.model.RM

both.eq <- which.min(abs(Sens.model.RM@y.values[[1]]-Spec.model.RM@y.values[[1]]))
Sens.model.RM@x.values[[1]][both.eq] ### Threshold value where sensitivity equals specificity
plot(Sens.model.RM, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.model.RM, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = 0.388, col = "black", lty = 3)

library(ModelMetrics) ## Now calculate the Kappa and TSS values
sdm.kappa=kappa(sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.nearrand.pseuabs.testdata$pred, cutoff = 0.388)
sdm.kappa
sdm.sens=sensitivity(sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.nearrand.pseuabs.testdata$pred, cutoff = 0.388)
sdm.sens
sdm.spec=sensitivity(sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.nearrand.pseuabs.testdata$pred, cutoff = 0.388)
sdm.spec
sdm.tss=sdm.sens+sdm.spec-1
sdm.tss

folds=trial.blocks.nearrandPO$folds

for(k in seq_len(length(folds))){
# extracting the training and testing indices
# this way works with folds list (but not foldID)
trainSet <- unlist(folds[[k]][1]) # training set indices
testSet <- unlist(folds[[k]][2]) # testing set indices
rf <- randomForest(as.factor(EWMSTATUS_corrRelFrq)~., sdm.ac.nearrand.pseuabs.data.df2[trainSet, ], ntree = 250) # model fitting on training set
sdm.nearrand.pseuabs.testdata$pred[testSet] <- predict(rf, sdm.ac.nearrand.pseuabs.data.df2[testSet, ], type = "prob")[,2] # predict the test set
}
precrec_obj.POnearrand = evalmod(scores = sdm.nearrand.pseuabs.testdata$pred, labels = sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq)
precrec_obj.POnearrand

sdm.pred.NearrandPO=prediction(sdm.nearrand.pseuabs.testdata$pred, sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq) ## make a prediction object
Sens.model.RM = performance(sdm.pred.NearrandPO,  measure="sens", x.measure="cutoff")
Sens.model.RM
Spec.model.RM = performance(sdm.pred.NearrandPO,  measure="spec", x.measure="cutoff")
Spec.model.RM
both.eq <- which.min(abs(Sens.model.RM@y.values[[1]]-Spec.model.RM@y.values[[1]]))
Sens.model.RM@x.values[[1]][both.eq] ### Threshold value where sensitivity equals specificity
plot(Sens.model.RM, type="l", col="red",xlab="",ylab="")
par(new=TRUE)
plot(Spec.model.RM, type="l", col="blue", xlab="Probability cutoff (threshold)",ylab="Sensitivity/Specificity")
abline(v = 0.384, col = "black", lty = 3)
sdm.kappa=kappa(sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.nearrand.pseuabs.testdata$pred, cutoff = 0.384)
sdm.kappa
sdm.sens=sensitivity(sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.nearrand.pseuabs.testdata$pred, cutoff = 0.384)
sdm.sens
sdm.spec=sensitivity(sdm.nearrand.pseuabs.testdata$EWMSTATUS_corrRelFrq,sdm.nearrand.pseuabs.testdata$pred, cutoff = 0.384)
sdm.spec
sdm.tss=sdm.sens+sdm.spec-1
sdm.tss

