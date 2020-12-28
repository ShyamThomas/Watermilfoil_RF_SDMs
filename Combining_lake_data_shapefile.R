library(sf)
library(rgdal)
library(dplyr)
library(tidyr)
library(readr)
setwd("~/UMNpostdoc/ProjectEWM/MinnEWM")

###Kelsey's base shapefile for all true lakes (from DNR?)
Lakes=readRDS("MinnGISlayers/mndow_lakes_sf_allDataUntransformed.rds")
class(Lakes) ### an sf object
st_write(Lakes,"Lakes.shp") ### the shapefile  was loaded on QGIS for visual mapping and correction

### extract the dataframe component of the sf object 
Lakes.df=Lakes %>% st_drop_geometry()
head(Lakes.df)
length(unique(Lakes.df$dowlknum)) ##19250 unique DOWs
Lakes.df$dowlknum[duplicated(Lakes.df$dowlknum)]###There are several duplicate row with same DOW's, most fall along the state boundary
###lines when crosschecked using QGIS...which were corrected on excel

write.csv(Lakes.df,"MinnEWMdata/DNRLakes_KelseyFile.csv") ##write the dataframe component of the shape file as csv for correction in excel itself
DNRLakes_K=read.csv("DNRLakes_KelseyFile.csv") ## read the corrected file with a new name
tail(DNRLakes_K) ## all duplicates removed
length(unique(Lakes.df$dowlknum)) ## 19164 unique DOWs
####A difference of 86 lake polygons (19250-19164)

###Now read the corrected dataframe as an sf (spatial) object, to do that we need to set a default CRS
projcrs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
DNRLakesK.sf = st_as_sf(x = DNRLakes_K,coords = c("INSIDE_X", "INSIDE_Y"),crs = projcrs)
class(DNRLakesK.sf)

###Finally the new sf object can be converted to a shapefile
st_write(DNRLakesK.sf,"DNRLakes_K.shp")

############################################## MERGE Lakes data from Kelsey with lake Max. depth data ################################################### 
Lakes_K=read.csv("MinnEWMdata/Lakes_level1_K.csv")
LakeMaxDepth.data=read.csv("MinnEWMdata/depths_for_GH_MNDOW.csv")
LakeMaxDepth.data=LakeMaxDepth.data[,c(2,4)]
head(LakeMaxDepth.data)
head(Lakes_K)
Lakes_MaxDepth.lj=left_join(Lakes_K, LakeMaxDepth.data, by="DOWLKNUM")
colnames(Lakes_MaxDepth.lj)[12]="DepthMax"

write.csv(Lakes_MaxDepth.lj,"MinnEWMdata/Lakes.MaxDepth.csv")
LakeDepthMax=read.csv("MinnEWMdata/Lakes.MaxDepth.csv")
headTail(LakeDepthMax)
View(LakeDepthMax)

#########################################################################################################################################################
#########################################################################################################################################################

#### We still don't have mean depth measure, so lets use the hypsography database
#### Hypsography data is stored as sperate csv files in the folder "hypsos"

hypsos.files = list.files(path="MinnEWMdata/hypsos",full.names = TRUE) %>%
lapply(function(x) {a <- read_csv(x);
mutate(a, id = tools::file_path_sans_ext(basename(x)))}) %>%
bind_rows

head(hypsos.files)
tail(hypsos.files)
hypsos.files.df=data.frame(hypsos.files)

group_by(id) %>%
summarise(
meanDEPTH=mean(depths),
maxDEPTH=max(depths),
maxAREA=max(areas))
head(hypsos.mean.byDOW)
hypsos.mean.byDOW.df=data.frame(hypsos.mean.byDOW)
head(hypsos.mean.byDOW.df)

###Need to parse DOWLKNUM from the "id" column
hypsos.mean.byDOW.df$DOWLKNUM=gsub("[^[:digit:]]", "", hypsos.mean.byDOW.df$i)
tail(hypsos.mean.byDOW.df)
hypsos.mean.byDOW.df.reorder=hypsos.mean.byDOW.df[,c(1,5,2,3,4)] # a final reordering of columns
write.csv(hypsos.mean.byDOW.df.reorder, "MinnEWMdata/meanDepth_fromHypsos.csv")

######################################################################################################################################
#### Merge mean depth with the LakeDepthMax database
LakeMeanDepth=read.csv("MinnEWMdata/meanDepth_fromHypsos.csv")
head(LakeMeanDepth)
LakeMeanDepth=LakeMeanDepth[,c(3,4)]## slect only what we need
head(LakeMeanDepth)
View(LakeMeanDepth)
length(LakeMeanDepth$dowlknum)

LakeDepthMax=read.csv("MinnEWMdata/Lakes.MaxDepth.csv")
headTail(LakeDepthMax)
length(LakeDepthMax$DOWLKNUM)

### Now merge the above datasets, go with full join to ensure all possible combinations are retained

LakeDepthMax_MeanDepth.fj=full_join(LakeDepthMax,LakeMeanDepth, by="DOWLKNUM")
headTail(LakeDepthMax_MeanDepth.fj)

###Mean depth is in feet while max depth is in meters
LakeDepthMax_MeanDepth.fj$DepthMean=0.3408*LakeDepthMax_MeanDepth.fj$meanDEPTH
headTail(LakeDepthMax_MeanDepth.fj)
length(LakeDepthMax_MeanDepth.fj)
LakeDepthMax_MeanDepth.fj=LakeDepthMax_MeanDepth.fj[,-13] ## remove the mean depth in feet and retain only meters
headTail(LakeDepthMax_MeanDepth.fj)
View(LakeDepthMax_MeanDepth.fj)

write_csv(LakeDepthMax_MeanDepth.fj, "MinnEWMdata/LakeMorphoMetrics.csv")

##And that's it!! We have a Lake Morphometrics dataset!! (spotted a few erroneous DOWLKNUMs)!!
##Corrected the errors in DOWLKNUMs; final data can be read from local drive:

LakeMorpho=read_csv("MinnEWMdata/LakeMorphoMetrics.csv")








######################################################################################################################################
##########################################################################################################################################################
##########################################################################################################################################################
#### I repeat the lake shapefile manipulation steps for another lake that I downloaded form DNR spatialcommons; its  loaded on QGIS for visual mapping
#### NOTE: this shapefile and its data were not used eventually
DNR_DOWLKNUM=st_read("MinnGISlayers/MNDNRHydro_DOWLKNUM.shp")
class(DNR_DOWLKNUM)
DNR_DOWLKNUM.df=DNR_DOWLKNUM %>% st_drop_geometry()class(DNR_DOWLKNUM.df)## Again stracting the dataframe out of the sf object
head(DNR_DOWLKNUM.df)
length(DNR_DOWLKNUM.df$dowlknum)
length(unique(DNR_DOWLKNUM.df$dowlknum)) ###26634 unique DOWs

DNR_DOWLKNUM.df$dowlknum[duplicated(DNR_DOWLKNUM.df$dowlknum)]##Lets see which are these duplicate DOWs and crosscheck them visually
### on QGIS
write.csv(DNR_DOWLKNUM.df, "DNR_DOWLKNUM.df.csv")## saved the data as csv and corrected all duplicates using excel functions
DNR_DOWLKNUMS.NODUPLICATES=read.csv("DNR_DOWLKNUM.df.csv")##read the corrected file with a new name
length(DNR_DOWLKNUMS.NODUPLICATES$dowlknum) ###26517 unique DOWs
###26634-26517=117## the number of duplicates

DNRLakes.sf = st_as_sf(x = DNR_DOWLKNUMS.NODUPLICATES,
                       coords = c("INSIDE_X", "INSIDE_Y"),
                       crs = projcrs)
class(DNRLakes.sf)










