
library(dplyr)
library(tidyr)
library(readr)
library(psych)

#######################################################################################################################################
############################################AGGREGATE CDOM (from Kelsey) DATA BY DOWLKNUM########################################################

CDOM.data=read.csv("MinnEWMdata/CDOM_data.csv")
str(CDOM.data)
CDOM.data$dowlknum_1[duplicated(CDOM.data$dowlknum_1)]

CDOM.data.aggbyDOW=CDOM.data %>%
  group_by(dowlknum_1) %>%
  summarise(
    X_UTM=mean(X_UTM),
    Y_UTM=mean(Y_UTM),
    Area=sum(PolyAcres),
    TotPix=sum(tot_pix),
    p2015_pix=mean(pix_2015p),
    p2016_pix=mean(pix2016p),
    a2015_a440=mean(a440_2015),
    a2016_a440=mean(a440_2016),
    ParentDOW=min(ParentDOW)
  )
CDOM.data.aggbyDOW.df=as.data.frame(CDOM.data.aggbyDOW)
head(CDOM.data.aggbyDOW.df)

colnames(CDOM.data.aggbyDOW.df)[1]="DOWLKNUM"

length(CDOM.data.aggbyDOW.df$DOWLKNUM)
head(CDOM.data.aggbyDOW.df)

###Use tidyr to change data from wide to long format
keycol = "Year_CDOMpar"
valuecol = "CDOMmeasure"
gathercols = c("p2015_pix", "p2016_pix", "a2015_a440","a2016_a440")
CDOM.data.aggbyDOW.df_long=gather_(CDOM.data.aggbyDOW.df, keycol, valuecol, gathercols)
head(CDOM.data.aggbyDOW.df_long)
###Another reordering by DOWLKNUM and Year_CDOMpar 
CDOM.data.aggbyDOW.df_long2 = CDOM.data.aggbyDOW.df_long[order(CDOM.data.aggbyDOW.df_long$DOWLKNUM, 
                                                               CDOM.data.aggbyDOW.df_long$Year_CDOMpar), ]
head(CDOM.data.aggbyDOW.df_long2)
###Split the Year_CDOMpar into 2 columns - Year and CDOMpar
###Get the Year column by using the substring function on Year_CDOMpar
Year=substring(CDOM.data.aggbyDOW.df_long2$Year_CDOMpar,2,5)
length(Year)
head(Year)
length(CDOM.data.aggbyDOW.df_long2[,1])
CDOM.data.aggbyDOW.df_long2$Year=Year
head(CDOM.data.aggbyDOW.df_long2)
###Similarly get CDOMpar column by using substring again on Year_CDOMpar
CDOMpar=substring(CDOM.data.aggbyDOW.df_long2$Year_CDOMpar,7,10)
head(CDOMpar)
CDOM.data.aggbyDOW.df_long2$CDOMpar=CDOMpar

###The final database with two newly created Year and CDOMpar columns
head(CDOM.data.aggbyDOW.df_long2)
str(CDOM.data.aggbyDOW.df_long2)
###One last step rearrange the columns in a better way
CDOM.data.aggbyDOW.df_long2=CDOM.data.aggbyDOW.df_long2[c(1,9,2:6,10,8,7)]
head(CDOM.data.aggbyDOW.df_long2)
CDOM.data.aggbyDOW.df_long2=CDOM.data.aggbyDOW.df_long2[c(1:5,7,6,8:10)]
head(CDOM.data.aggbyDOW.df_long2)### Okay!! Finallt have it in a better order
write.csv(CDOM.data.aggbyDOW.df_long2,"MinnEWMdata/CDOMdata_aggbyDOWYEAR.csv")

#### Chnanged again the dataset by splitting the CDOMpar column by their  two categories - a440 and pix
CDOM.data.byDOW=read.csv("MinnEWMdata/CDOMdata_aggbyDOWYEAR.csv")
headTail(CDOM.data.byDOW)
CDOM.data.byDOW=CDOM.data.byDOW[,-10]
head(CDOM.data.byDOW)
CDOM.data.byDOW_wide=as.data.frame(CDOM.data.byDOW %>%
                                     pivot_wider(names_from = CDOMpar, values_from = CDOMmeasure)
)
head(CDOM.data.byDOW_wide)
write_csv(CDOM.data.byDOW_wide ,"MinnEWMdata/CDOMdata_aggbyDOWYEAR.csv") ### overwrite the previously saved csv file

#### Summarize the last created dataset by lakeid: DOW only
head(CDOM.data.byDOW_wide)

CDOM.data.byDOW =CDOM.data.byDOW_wide %>%
  group_by(DOWLKNUM) %>%
  summarise(
    X_UTM=mean(X_UTM),
    Y_UTM=mean(Y_UTM),
    Area=mean(Area),
    TotPix=mean(TotPix),
    pix=mean(pix),
    a440=mean(a440),
    ParentDOW=min(ParentDOW),
    minYear=min(YEAR),
    maxYear=max(YEAR)
  )
CDOM.data.byDOW.df=data.frame(CDOM.data.byDOW)
headTail(CDOM.data.byDOW)
write_csv(CDOM.data.byDOW ,"MinnEWMdata/CDOMdata_aggbyDOW.csv")

#########################################################################################################################################################
#################################################SECCHI BY YEAR DATABASE (from Kelsey again)############################################################
Secchi.data=read.csv("MinnEWMdata/SecchiData_1995.csv")
headTail(Secchi.data)
### summarize by DOW and YEAR
Secchi.data_aggbyDOWyear=Secchi.data %>%
  group_by(DOWLKNUM,Year) %>%
  summarise(
    avgSecchi.m=mean(secchi.m),
    minSecchi.m=min(secchi.m),
    maxSecchi.m=max(secchi.m)
  )
Secchi.data_aggbyDOWyear.df=data.frame(Secchi.data_aggbyDOWyear)
tail(Secchi.data_aggbyDOWyear.df)
write.csv(Secchi.data_aggbyDOWyear.df,"MinnEWMdata/Secchi.data1995_aggbyDOWYEAR.csv")

### summarize by DOW alone
Secchi.data_aggbyDOW=Secchi.data %>%
  group_by(DOWLKNUM) %>%
  summarise(
    avgSecchi.m=mean(secchi.m),
    minSecchi.m=min(secchi.m),
    maxSecchi.m=max(secchi.m)
  )
Secchi.data_aggbyDOW.df=data.frame(Secchi.data_aggbyDOW)
headTail(Secchi.data_aggbyDOW.df)
write_csv(Secchi.data_aggbyDOW.df,"MinnEWMdata/Secchi.data1995_aggbyDOW.csv")

#########################################################################################################################################################
########################################################### MERGE ALL THE CDOM and SECCHI FILES CREATED #################################################
###Lets start with files that have both DOW and sampling Year as attributes, since our final database has to be based on lakeID-year and LakeID alone

Secchidata_aggbyDOWYEAR=read_csv("MinnEWMdata/Secchi.data1995_aggbyDOWYEAR.csv")
headTail(Secchidata_aggbyDOWYEAR)

CDOMdata_aggbyDOWYEAR=read_csv("MinnEWMdata/CDOMdata_aggbyDOWYEAR.csv")
headTail(CDOMdata_aggbyDOWYEAR)

CDOMSecchi.MergedbyDOWYEAR=full_join(Secchidata_aggbyDOWYEAR,CDOMdata_aggbyDOWYEAR, by=c("DOWLKNUM","YEAR"))
headTail(CDOMSecchi.MergedbyDOWYEAR)
View(CDOMSecchi.MergedbyDOWYEAR)
write_csv(CDOMSecchi.MergedbyDOWYEAR,"MinnEWMdata/SecchiCDOM.MergedbyDOWYEAR.csv")

### Merge based on LakeId alone
Secchidata_aggbyDOW=read_csv("MinnEWMdata/Secchi.data1995_aggbyDOW.csv")
headTail(Secchidata_aggbyDOW)

CDOMdata_aggbyDOW=read_csv("MinnEWMdata/CDOMdata_aggbyDOW.csv")
headTail(CDOMdata_aggbyDOW)

CDOMSecchi.MergedbyDOW=full_join(Secchidata_aggbyDOW,CDOMdata_aggbyDOW, by=c("DOWLKNUM"))
View(CDOMSecchi.MergedbyDOW)
write_csv(CDOMSecchi.MergedbyDOW,"MinnEWMdata/SecchiCDOM.MergedbyDOW.csv")








#########################################################################################################################################################
########################################################################################################################################################
##### Now let's merge data that have no year associated with it...

Lakes_K=read.csv("MinnEWMdata/Lakes_level1_K.csv")
LakeMaxDepth.data=read.csv("MinnEWMdata/depths_for_GH_MNDOW.csv")
LakeMaxDepth.data=LakeMaxDepth.data[,c(2,4)]

head(LakeMaxDepth.data)
head(Lakes_K)
Lakes_MaxDepth.lj=left_join(Lakes_K, LakeMaxDepth.data, by="DOWLKNUM")
head(Lakes_MaxDepth.lj)
tail(Lakes_MaxDepth.lj)
length(Lakes_MaxDepth.lj)
colnames(Lakes_MaxDepth.lj)[12]
colnames(Lakes_MaxDepth.lj)[12]="DepthMax"
tail(Lakes_MaxDepth.lj)
write.csv(Lakes_MaxDepth.lj,"MinnEWMdata/Lakes.MaxDepth.csv")

LakeDepthMax=read.csv("MinnEWMdata/Lakes.MaxDepth.csv")
head(LakeDepthMax)
Secchi.CDOM=read.csv("MinnEWMdata/Secchi_CDOM.csv")
tail(Secchi.CDOM)
length(unique(Secchi.CDOM$DOWLKNUM))
Secchi.CDOM=read.csv("MinnEWMdata/Secchi_CDOM.csv", row.names = "FALSE")
length(unique(LakeDepthMax$DOWLKNUM))
LakeMaxDepth.lj.SecchiCDOM=left_join(Secchi.CDOM, LakeDepthMax, by="DOWLKNUM")
tail(LakeMaxDepth.lj.SecchiCDOM)
write.csv(LakeMaxDepth.lj.SecchiCDOM,"MinnEWMdata/Secchi.CDOM.LakeDepth.csv")