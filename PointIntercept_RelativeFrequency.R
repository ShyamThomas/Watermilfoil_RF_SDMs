library(tidyverse)
library(reshape2)

PointData=read_csv("MinnEWMdata/mn_plant_survey.csv")ci #### the point intercept data from recent surveys by Muthukrishnan et al.
                                                      #### have some discrepancy in DOWLKNUM ids
PointData
dim(PointData)
length(unique(PointData$DOWLKNUM)) ### 1554 lakes

PointData$DOWLKNUM=as.numeric(PointData$DOWLKNUM) ### this corrects DOWLKNUMs that don't match well with Muthukrishnan et al. id's

PointData_narmd=PointData%>% ### This is the corrected point intercept data after removing  erroneous DOWLKNUMs
filter(!is.na(DOWLKNUM))

PointData_narmd
length(unique(PointData_narmd$DOWLKNUM)) ### 1534 lakes, 20 lake ids from Muthukrishnan et al. don't match

PointData_mys=PointData_narmd %>%
filter(TAXON=="Myriophyllum spicatum")
PointData_mys
length(unique(PointData_mys$DOWLKNUM)) ### 154 lakes with EWM (Myriophyllum spicatum aka mys); and a lot of surveyed absences for EWM!!


#### Getting the proportion of point intercepts within lakes that have EWM: a relative frequency measure of EWM abundance
#### Lake-specific EWM relative frequency: sum of point intercepts in a lake with EWM record/total number lake point intercept samples
PointData_wide=dcast(PointData_narmd, DOWLKNUM + DEPTH_FT+STA_NBR + SURVEY_ID + POINT_ID ~ TAXON, value.var = "POINT_ID")
head(PointData_wide)
dim(PointData_wide)

PointData_wide_mys=subset(PointData_wide,select = c("DOWLKNUM", "DEPTH_FT", 
                                                    "STA_NBR", "SURVEY_ID", "POINT_ID", "Myriophyllum spicatum"))
head(PointData_wide_mys)
colnames(PointData_wide_mys)[6]="MYS"
head(PointData_wide_mys)

MYS_pointdata_agg=PointData_wide_mys%>%
group_by(DOWLKNUM, SURVEY_ID)%>% ### note some lakes (DOWs) have been surveyed more than once (SURVEYIDs); multiple survey ids for same lake!
summarise(
TotalMYS=sum(MYS),
TotalPointId=length(unique(POINT_ID)),
MeanDepth = mean(DEPTH_FT),
MaxDepth = max(DEPTH_FT)
)
MYS_pointdata_agg

MYS_pointdata_agg$MYS_relfrq=MYS_pointdata_agg$TotalMYS/MYS_pointdata_agg$TotalPointId
MYS_pointdata_agg
length(unique(MYS_pointdata_agg$DOWLKNUM)) #### 1534 unique lakes

MYS_rel.point.abs=MYS_pointdata_agg%>%
filter(MYS_relfrq == 0)
MYS_rel.point.abs

MYS_rel.point.abun=MYS_pointdata_agg%>%
filter(MYS_relfrq > 0)
MYS_rel.point.abun

#### Merge the point intercept data with EWM infestation dataset
EWM.data=read_csv("MinnEWMdata/MergedData/EWMinfestation/EWMinf.preds.data.csv") ### the full EWM infestation data with all lakes possible
EWM.data.selvars2=EWM.data[,-c(8:11)] ### removed variables associated with EWM infestation dataset
dim(EWM.data.selvars2)
View(EWM.data.selvars2)

length(intersect(MYS_pointdata_agg$DOWLKNUM,EWM.data.selvars2$DOWLKNUM)) ### 1473 lakes in common out of 1534 point intercept  lakes
length(setdiff(MYS_pointdata_agg$DOWLKNUM,EWM.data.selvars2$DOWLKNUM)) #### hence, 61 lakes have no corresponding match in the 
                                                                       ####infested lakes file!

EWM.data.selvars2$DOWLKNUM=as.numeric(EWM.data.selvars2$DOWLKNUM)
EWM.relfrq.preds.data=left_join(MYS_pointdata_agg,EWM.data.selvars2, by="DOWLKNUM") ### merge with the lake file
EWM.relfrq.preds.data.narmd=EWM.relfrq.preds.data%>% ### the 61 lakes in the point intercept data that have no match is removed
filter(LAT!= "NA")
View(EWM.relfrq.preds.data.narmd)

write_csv(EWM.relfrq.preds.data.narmd, "MinnEWMdata/EWMrelfrq_infes.preds.data.csv") ##

######## More selections and subsetting for the final GAM-ready dataset!
selvars=c('DOWLKNUM', 'MYS_relfrq', 'LON','LAT', 'EWMSTATUS', 'size_acres', 'max_depth', 'avg_totalalkalinity', 
          'avg_ph','avg_secchi','avg_conductance','avg_nitrogen','avg_phosphorus', 'avg_chlorophylla', 'a440.cdom', 'mean.gdd_wtr_10c')
EWM.relfrq.preds.data.narmd.selvars=EWM.relfrq.preds.data.narmd[selvars]

EWM.relfrq.preds.data.narmd.selvars2=EWM.relfrq.preds.data.narmd.selvars%>%
select(-c(avg_totalalkalinity,avg_nitrogen))%>%
na.omit()

EWM.relfrq.preds.data.narmd.selvars2 ### The final data that goes in a GAM model with relative frequency of occurrences as response
                                     ### after correcting for all NAs in water chemistry data only 396 records
  
EWM.relfrq.preds.data.narmd.selvars2.abs=EWM.relfrq.preds.data.narmd.selvars2%>%
filter(MYS_relfrq ==0)
EWM.relfrq.sel.abs.sf=st_as_sf(EWM.relfrq.preds.data.narmd.selvars2.abs, coords=c("LON", "LAT"), crs=st_crs(Minn.sf))

EWM.relfrq.selvars2.abs.map=ggplot()+
geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
geom_sf(data=EWM.relfrq.sel.abs.sf, color="black", pch=1)+ theme(legend.title = element_blank())
EWM.relfrq.selvars2.abs.map

EWM.relfrq.preds.data.narmd.selvars2.abun=EWM.relfrq.preds.data.narmd.selvars2%>%
filter(MYS_relfrq >0)
EWM.relfrq.sel.abun.sf=st_as_sf(EWM.relfrq.preds.data.narmd.selvars2.abun, coords=c("LON", "LAT"), crs=st_crs(Minn.sf))
EWM.relfrq.selvars2.abun.map=ggplot()+
geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
geom_sf(data=EWM.relfrq.sel.abun.sf, aes(color=MYS_relfrq))+
scale_color_gradientn(colours = topo.colors(10))+ theme(legend.title = element_blank())
EWM.relfrq.selvars2.abun.map



#### Filtering out all positive records from the point intercept data
EWM.relfrq.abun=EWM.relfrq.preds.data.narmd%>%
filter(MYS_relfrq > 0)
EWM.relfrq.abun

EWM.relfrq.abs=EWM.relfrq.preds.data.narmd%>%
filter(MYS_relfrq == 0)
EWM.relfrq.abs

#### Convert the extracted abundance data to a sf obect for mapping
Minn.sf=read_sf(dsn="/Users/thom7552/UMNpostdoc/ProjectEWM/MinnEWM/MinnGISlayers", layer="Minn.map")

EWM.relfrq.abun.sf=st_as_sf(EWM.relfrq.abun, coords=c("LON", "LAT"), crs=st_crs(Minn.sf))

EWM.abun.map=ggplot()+
geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
geom_sf(data=EWM.relfrq.abun.sf$geometry, aes(color=EWM.relfrq.abun.sf$MYS_relfrq))+
scale_color_gradientn(colours = topo.colors(10))+ theme(legend.title = element_blank())


EWM.abun.map ### the distribution of EWM abundance data overlaps quite well with the presence distribution from EWM infestation data... Yay!

png("MinnEWMfigures/EWM.abun.map.png",units="in", width=5, height=6, res=350) ### saved the map.



######################################## A generic skimr function to summarize the point intercept abundance data
library(skimr)

my_skim = skim_with(numeric = sfl(hist=NULL), base = sfl(
  missing = n_missing,
  complete = n_complete,
  n = length
))
my_skim(EWM.relfrq.preds.data2) ### Copy pasted the table to excel summary file

#########################################################################################################################################
################ Extract abundance data for other closely associated co-occurring species within Myriophyllum genus ####################
head(PointData_wide)

PointData_wide_mysib=subset(PointData_wide,select = c("DOWLKNUM", "DEPTH_FT",
"STA_NBR", "SURVEY_ID", "POINT_ID", "Myriophyllum sibiricum"))
head(PointData_wide_mysib)
colnames(PointData_wide_mysib)[6]="MYSIB"
head(PointData_wide_mysib)


MYSIB_pointdata_agg=PointData_wide_mysib%>%
group_by(DOWLKNUM, SURVEY_ID)%>%
summarise(
TotalMYSIB=sum(MYSIB),
TotalPointId=length(unique(POINT_ID)),
MeanDepth = mean(DEPTH_FT),
MaxDepth = max(DEPTH_FT)
)
MYSIB_pointdata_agg$MYSIB_relfrq=MYSIB_pointdata_agg$TotalMYSIB/MYSIB_pointdata_agg$TotalPointId
MYSIB_pointdata_agg

MYSIB_rel.point.abun=MYSIB_pointdata_agg%>%
  filter(MYSIB_relfrq > 0)
MYS_rel.point.abun
hist(MYSIB_rel.point.abun$MYSIB_relfrq)

MYS_MYSIB_pointdata=left_join(MYS_pointdata_agg,MYSIB_pointdata_agg, by="DOWLKNUM")

library(quantreg) ### load quantile regression package

mys_mysib_corrplot=ggplot(data=MYS_MYSIB_pointdata, aes(x=MYSIB_relfrq, y=MYS_relfrq))+
geom_point(colour="dark gray", alpha=0.33)+
geom_quantile(quantiles=qs, formula=y ~ poly(x, 3), colour="red")+
geom_smooth(method="gam", se=FALSE, colour="black")+xlab("M. sibiricum relative frequency")+ylab("M. spicatum relative frequency")+
theme_classic()
png("MinnEWMfigures/PointData_CorrPlot_M.spic_M.sib.png",units="in", width=5, height=5, res=350)

######### Repeat the above steps for all native species that belong to Myriophyllum genus
head(PointData_wide)

PointData_wide_AllNativeMyro=subset(PointData_wide,select = c("DOWLKNUM", "DEPTH_FT",
"STA_NBR", "SURVEY_ID", "POINT_ID", "Myriophyllum sibiricum", "Myriophyllum alterniflorum", "Myriophyllum exalbescens",
"Myriophyllum farwellii", "Myriophyllum tenellum", "Myriophyllum verticillatum"))
head(PointData_wide_AllNativeMyro)
colnames(PointData_wide_AllNativeMyro)[6:11]= c("MYSIB", "MYALT", "MYEXA", "MYFAR", "MYTEN", "MYVER")
head(PointData_wide_AllNativeMyro)

MYS_pointdata_agg

NativeMYS_pointdata_agg=PointData_wide_AllNativeMyro%>%
group_by(DOWLKNUM, SURVEY_ID)%>%
summarise(
TotalMYSIB=sum(MYSIB),
TotalMYALT=sum(MYALT),
TotalMYEXA=sum(MYEXA),
TotalMYFAR=sum(MYFAR),
TotalMYTEN=sum(MYTEN),
TotalMYVER=sum(MYVER),
TotalPointId=length(unique(POINT_ID)),
MeanDepth = mean(DEPTH_FT),
MaxDepth = max(DEPTH_FT)
)
NativeMYS_pointdata_sum=NativeMYS_pointdata_agg %>% mutate(sumrow= TotalMYSIB+TotalMYALT+TotalMYEXA+TotalMYFAR+ TotalMYTEN+ TotalMYVER)
colnames(NativeMYS_pointdata_sum)[12]="TotalNativeMyrio"
MYS_NatMyrio_pointdata=left_join(MYS_pointdata_agg,NativeMYS_pointdata_sum, by="DOWLKNUM")
MYS_NatMyrio_pointdata

MYS_NatMyrio_pointdata$PrpnNativeMyrio=MYS_NatMyrio_pointdata$TotalNativeMyrio/MYS_NatMyrio_pointdata$TotalPointId.y

#### Plot the relation
mys_natmy_corrplot=ggplot(data=MYS_NatMyrio_pointdata, aes(x=PrpnNativeMyrio, y=MYS_relfrq))+
geom_point(colour="dark gray", alpha=0.33)+
geom_quantile(quantiles=qs, formula=y ~ poly(x, 3), colour="red")+
geom_smooth(method="gam", formula =y ~ s(x, bs = "cs", k=3) , se=FALSE, colour="black")+xlab("Native Myriophyllum sp. relative frequency")+ylab("M. spicatum relative frequency")+theme_classic()
png("MinnEWMfigures/PointData_CorrPlot_M.spic_NativeMyrio.png",units="in", width=5, height=5, res=350)
mys_natmy_corrplot
dev.off()

#### A  final zoomed-in plots of both EWM relative frequencey correlations with M. sibiricum and all native Myriophyllum sp.
grid.arrange(mys_mysib_corrplot+coord_cartesian(xlim=c(0.0,0.5),ylim=c(0.0,0.5)), 
             mys_natmy_corrplot+coord_cartesian(xlim=c(0.0,0.5),ylim=c(0.0,0.5)),ncol=2)


