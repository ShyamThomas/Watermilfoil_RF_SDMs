
### Code from Holly for fixing the lake DOW for the DNR's Infested Waters List. Script source from Holly Kundel
library(readr) 
library(readxl)
library(dplyr)
library(stringr)

#### infested_waters <- read_csv('ZM_WAE_Summaries/infested_waters.csv') # ,if saved as a csv change the file name and file path to match th file name and where you saved it on your computer
infested_waters<- read_excel('infested_waters.xlsx') # or read it in as an excel file

### Renaming columns for easier use in future and remove extra column
IW <- infested_waters %>% 
  rename(dow = 6) %>% # these renames basically make it easier to work with the columns
  rename(Water_Body_Name = 1) %>%
  rename (County = 2) %>%
  rename(AIS_Species = 3) %>%
  rename(Year_Infested = 4)%>%
  rename (Year_Confirmed = 5)%>%
  select(dow,Water_Body_Name,County,AIS_Species,Year_Infested,Year_Confirmed)%>% # this just selected the columns that I wanted, you can select as many or few as you want
  filter(!dow == "none")%>% #removes rows where DOW is listed as "none"
  filter(!dow == "none, part of Winnibigoshish") # not sure why this isn't listed as Winnibigoshish...

#### Must reformat DOW (currently 6 digits with dash, we want it to be 7 or 8 digits no dashes)
IW_fixed <- IW %>% mutate(dow = gsub("-","",IW$dow)) %>% # removes all dashes from dow IW_nodash$dow
  mutate(DOW = str_pad(dow,8, side="right", pad= "0")) %>% # This adds zeroes so that all DOWs are 8 digits
  rename(parent_dow=dow) # sometimes DNR calls the first 6 digits of the dow the parent dow, this ignores the last two digits that identify sub-basins

IW_parent_dow <- IW_fixed %>% # ensures everything is the same length with no additional spaces
  mutate(PARENT_DOW2 = str_trunc(IW_fixed$DOW, ellipsis= "", side = "right",6))%>%
  mutate(PARENT_DOW = str_trim(PARENT_DOW2, side = "right"))%>%
  select(-PARENT_DOW2, - parent_dow, -Year_Confirmed)

# This is my code to just get zebra mussel data
ZM <- IW_parent_dow %>% 
  filter(AIS_Species== "zebra mussel") %>%
  filter(!is.na(DOW)) #filters out rivers and creeks (anything without a DOW)

# To get just Eurasian watermilfoil:
EW <- IW_parent_dow %>%
  filter(AIS_Species == "Eurasian watermilfoil")%>%
  filter(!is.na(DOW)) 

NonEWM.infested_waters <- IW_parent_dow %>%
filter(AIS_Species!= "Eurasian watermilfoil") %>%
filter(!is.na(DOW)) #filters out rivers and creeks (anything without a DOW)
write.csv(NonEWM.infested_waters,"MinnEWMdata/EWMinfested_lakes.csv")

###############################################################################################
###Used the above codes to get EWM infested lakes and nonEWM infested lakes
NonEWMlakes=read.csv("MinnEWMdata/NonEWMinfested_lakes.csv")
head(NonEWMlakes)
EWMlakes=read.csv("MinnEWMdata/EWMinfested_lakes.csv")
head(EWMlakes)

###Find the nonEWM lakes that are uniquely EWM absence lakes
NonEWMlakes.unq=setdiff(NonEWMlakes$DOW, EWMlakes$DOW)
is.vector(NonEWMlakes.unq)
UnqNonEWMlakes=NonEWMlakes[NonEWMlakes$DOW %in% NonEWMlakes.unq, ]
tail(UnqNonEWMlakes)
write.csv(UnqNonEWMlakes,"MinnEWMdata/NonEWMlakes.trueabs.csv")

#################################################################################################
### Clean the infested and non-infested EWM lake datasets further because of duplications
NonEWMlakes=read.csv("MinnEWMdata/NonEWMlakes.trueabs.csv")
head(NonEWMlakes)
EWMlakes=read.csv("MinnEWMdata/EWMinfested_lakes.csv")
head(EWMlakes)

EWMlakes$DOWLKNUM[duplicated(EWMlakes$DOWLKNUM)] #### There are some duplications in both datasets
NonEWMlakes$DOWLKNUM[duplicated(NonEWMlakes$DOWLKNUM)] 

### Remove duplicates using "distinct"
NonEWMlakes.noduplicates=data.frame(
NonEWMlakes %>%
distinct(DOWLKNUM, .keep_all = TRUE)
)
tail(NonEWMlakes.noduplicates)

EWMlakes.noduplicates=data.frame(
EWMlakes %>%
distinct(DOWLKNUM, .keep_all = TRUE)
)
tail(EWMlakes.noduplicates)

###Save the lake infestation data without any duplication of DOWLKNUM
write.csv(EWMlakes.noduplicates, "MinnEWMdata/EWMinfest.noduplicates.csv")
write.csv(NonEWMlakes.noduplicates, "MinnEWMdata/NonEWMinfest.noduplicates.csv")

### Read and merge the above datasets to create EWM lake infestation status; infested is '1' and unifested is '0'
EWMinf.lakes=read.csv("MinnEWMdata/EWMinfest.noduplicates.csv")
NonEWMinf.lakes=read.csv("MinnEWMdata/NonEWMinfest.noduplicates.csv")

EWMinf.lake.status=rbind(EWMinf.lakes,NonEWMinf.lakes)
headTail(EWMinf.lake.status)
write.csv(EWMinf.lake.status,"MinnEWMdata/EWMinf.lake.status.csv") ### the final dataset with EWM invasion status

#########################################################################################################################################################
#########################################################################################################################################################

#### Create an index table that captures all lakes available (sourced from DNR hydrology shapefile) along with two distinct
#### columns that highlight subset of all lakes survey and a still smaller subset of lakes with positive EWM finds

###### All lakes available from DNR shapefile and Kelsey data corrected to include lakes only
Lakes_K=read_csv("MinnEWMdata/Lakes_level1_K.csv")
headTail(Lakes_K) ### 19164 unique DOWs

EWMinf.lake.status=read_csv("MinnEWMdata/EWMinf.lake.status.csv")
headTail(EWMinf.lake.status)

EWMinf.lake.status.sel=EWMinf.lake.status[,c(3:5,7)] ## only selecting few needed columns
headTail(EWMinf.lake.status.sel)
headTail(Lakes_K)

LakeIndexFile=full_join(Lakes_K, EWMinf.lake.status.sel, by="DOWLKNUM") ## An indexfile for all lakes shwoing survey and EWM status
LakeIndexFile$Status[is.na(LakeIndexFile$Status)] <- "U" ## Designated all NA's in the status to imply "Unsuryed"
View(LakeIndexFile)

write_csv(LakeIndexFile ,"MinnEWMdata/LakeIndexFile.csv") ##saved and made some final changes directly on Excel

EWMlake_indexfile=read_csv("MinnEWMdata/LakeIndexFile.csv") ##the final version of the EWMlake_indexfile
headTail(EWMlake_indexfile)
View(EWMlake_indexfile) ##Note an additional column: Surveyed ("Y" or "N") shows wether the lake was surveyed or not

#### The Index file created provides an oveall baseline for all other datasets (Morphometrics, CDOM & Secchi, Water chemistry)
#### Need to first check if the LakeIds of Index file contains all possible lakes coming from other datasets

###### First read all the files
LakeMorpho=read_csv("MinnEWMdata/LakeMorphoMetrics.csv")
SecchiCDOM.DOW=read_csv("MinnEWMdata/SecchiCDOM.MergedbyDOW.csv")
SecchiCDOM.DOWYEAR=read_csv("MinnEWMdata/SecchiCDOM.MergedbyDOWYEAR.csv")
Watchem.DOW=read_csv("MinnEWMdata/WatchemSummer.MergedbyDOW.csv")
Watchem.DOWYEAR=read_csv("MinnEWMdata/WatchemSummer.MergedbyDOWYEAR.csv")

##### The index file again
EWMlake_indexfile=read_csv("MinnEWMdata/LakeIndexFile.csv")

### Use the set difference function to ensure all lakes are represented by the Index file
##### First with lake morphometrics dataset 
length(setdiff(LakeMorpho$DOWLKNUM, EWMlake_indexfile$DOWLKNUM)) ### Zero! Yay!!
LakeMorpho.depth=subset(LakeMorpho[,c(1,12,13)])
tail(LakeMorpho.depth)
EWMlakeindex.morpho=full_join(EWMlake_indexfile, LakeMorpho.depth, by="DOWLKNUM")
length(EWMlakeindex.morpho$DOWLKNUM)
headTail(EWMlakeindex.morpho)

write_csv(EWMlakeindex.morpho,"MinnEWMdata/EWMlakeindex.morphometrics.csv")

###### Second using CDOM & Secchi dataset
######## First SecchiCDOM merged by DOWLKNUM only
tail(SecchiCDOM.DOW)
tail(EWMlake_indexfile)
EWMlakeindex.CDOMSecchi.DOW=left_join(EWMlake_indexfile,SecchiCDOM.DOW, by="DOWLKNUM") 
length(EWMlakeindex.CDOMSecchi.DOW$DOWLKNUM)
headTail(EWMlakeindex.CDOMSecchi.DOW)

write_csv(EWMlakeindex.CDOMSecchiDOW,"MinnEWMdata/EWMlakeindex.CDOMSecchi.byDOW.csv")
####### Now SecchiCDOM merged by DOWLKNUM and YEAR  
tail(SecchiCDOM.DOWYEAR)
tail(EWMlake_indexfile)
EWMlakeindex.CDOMSecchi.DOWYEAR=left_join(EWMlake_indexfile,SecchiCDOM.DOWYEAR, by="DOWLKNUM")
headTail(EWMlakeindex.CDOMSecchi.DOWYEAR)
View(EWMlakeindex.CDOMSecchi.DOWYEAR)

write_csv(EWMlakeindex.CDOMSecchi.DOWYEAR,"MinnEWMdata/EWMlakeindex.CDOMSecchi.byDOWYEAR.csv")
##############################################################################################
##### Finally merge using the Water chmistry data
######## Starting again with DOWLKNUM only
tail(Watchem.DOW)
EWMlakeindex.Watchem.DOW=left_join(EWMlake_indexfile,Watchem.DOW, by="DOWLKNUM")
headTail(EWMlakeindex.Watchem.DOW)
length(EWMlakeindex.Watchem.DOW$DOWLKNUM)
View(EWMlakeindex.Watchem.DOW)

write_csv(EWMlakeindex.Watchem.DOW,"MinnEWMdata/EWMlakeindex.Watchem.byDOW.csv")
###### Finally, merging Watchem DOW-YEAR data
EWMlakeindex.Watchem.DOWYEAR=left_join(EWMlake_indexfile,Watchem.DOWYEAR, by="DOWLKNUM")
View(EWMlakeindex.Watchem.DOWYEAR)
length(EWMlakeindex.Watchem.DOWYEAR$DOWLKNUM)

write_csv(EWMlakeindex.Watchem.DOWYEAR,"MinnEWMdata/EWMlakeindex.Watchem.byDOWYEAR.csv")
##################################################################################################################################
##################################################################################################################################
#### We finally need to merge all the datasets to make a single large dataset that combines lake index, morphometrics, secchi-cdom,
#### waterchemistry seperately by DOW and DOWYEAR

###### Start with DOW only datasets
#Watchem.DOW=read_csv("MinnEWMdata/WatchemSummer.MergedbyDOW.csv")
headTail(Watchem.DOW)
length(Watchem.DOW$DOWLKNUM)

#SecchiCDOM.DOW=read_csv("MinnEWMdata/SecchiCDOM.MergedbyDOW.csv")
headTail(SecchiCDOM.DOW)
length(SecchiCDOM.DOW$DOWLKNUM)

#### Merge the SecchiCDOM with Watchem first by DOWLKNUM
SecchiCDOM.Watchem.byDOW=full_join(SecchiCDOM.DOW, Watchem.DOW, by="DOWLKNUM")
headTail(SecchiCDOM.Watchem.byDOW)
length(SecchiCDOM.Watchem.byDOW$DOWLKNUM) 

#### Subset the dataframe to select only needed variables
SecchiCDOM.Watchem.byDOW.selvar=subset(SecchiCDOM.Watchem.byDOW[,-c(10:21,28:33)])
headTail(SecchiCDOM.Watchem.byDOW.selvar)

#### Finally merge the above with the lake index and morphometric datset
EWMlakeindex.Morpho=read_csv("MinnEWMdata/EWMlakeindex.morphometrics.csv")
headTail(EWMlakeindex.Morpho)
length(EWMlakeindex.Morpho$DOWLKNUM)

EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOW=left_join(EWMlakeindex.Morpho,SecchiCDOM.Watchem.byDOW.selvar, by="DOWLKNUM")
headTail(EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOW)
length(EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOW$DOWLKNUM)
##### The final large dataset by DOW
write_csv(EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOW, "MinnEWMdata/EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOW.csv")

###### After reaaranging and editing column titles on Excel, read the full dataset by DOW and explore...
EWM.fulldata=read_csv("MinnEWMdata/EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOW.csv")
headTail(EWM.fulldata)
EWM.pred.cor=round(cor(EWM.fulldata[,c(15:19,22:31)], use = "pairwise.complete.obs", method = "pearson"), 2)
EWM.pred.cor
corrplot(EWM.pred.cor, type = "upper", order = "hclust",
tl.col = "black", tl.srt = 90, method = "circle", number.cex = 0.5, addCoef.col = "black")

##################################################################################################################################
##################################################################################################################################
#### Repeat the above steps for DOW-YEAR
## Watchem.DOWYEAR=read_csv("MinnEWMdata/WatchemSummer.MergedbyDOWYEAR.csv")
headTail(Watchem.DOWYEAR)
length(Watchem.DOWYEAR$DOWLKNUM)
length(unique(Watchem.DOWYEAR$DOWLKNUM))

## SecchiCDOM.DOWYEAR=read_csv("MinnEWMdata/SecchiCDOM.MergedbyDOWYEAR.csv")
headTail(SecchiCDOM.DOWYEAR)
length(SecchiCDOM.DOWYEAR$DOWLKNUM)
length(unique(SecchiCDOM.DOWYEAR$DOWLKNUM))

#### Merge the SecchiCDOM with Watchem first by DOWLKNUM & SAMPLEYEAR
SecchiCDOM.Watchem.byDOWYEAR=full_join(SecchiCDOM.DOWYEAR, Watchem.DOWYEAR, by= c("DOWLKNUM", "SAMPLEYEAR"))
length(unique(SecchiCDOM.Watchem.byDOWYEAR$DOWLKNUM))

#### Subset the dataframe to select only needed variables
SecchiCDOM.Watchem.byDOWYEAR.selvar=subset(SecchiCDOM.Watchem.byDOWYEAR[,-c(11:22,29:34)])
headTail(SecchiCDOM.Watchem.byDOWYEAR.selvar)

#### Finally merge the above with the lake index and morphometric datset
## EWMlakeindex.Morpho=read_csv("MinnEWMdata/EWMlakeindex.morphometrics.csv")
headTail(EWMlakeindex.Morpho)
length(EWMlakeindex.Morpho)

EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOWYEAR=left_join(EWMlakeindex.Morpho,SecchiCDOM.Watchem.byDOWYEAR.selvar,
                                                   by="DOWLKNUM") ## left join to ensure only the index lakes are involved
                                                                  ## no. of rows >> lake index rows due to multiple years 
headTail(EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOWYEAR)
length(EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOWYEAR$DOWLKNUM)
length(unique(EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOWYEAR$DOWLKNUM))

##### The final large dataset by DOWYEAR
write_csv(EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOWYEAR, "MinnEWMdata/EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOWYEAR.csv")

###### After reaaranging and editing column titles on Excel, read the full dataset by DOWYEAR and explore...
EWM.fulldata2=read_csv("MinnEWMdata/EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOWYEAR.csv")

