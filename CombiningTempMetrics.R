
setwd("~/UMNpostdoc/ProjectEWM/MinnEWM")

library(tidyverse)
library(psych)
library(skimr)

NLDAS_thermal=read_tsv("MinnEWMdata/NLDAS_thermal_metrics.tsv")
NLDAS_thermal
LakeDOW.NHD=read_csv("MinnEWMdata/LakeDOW_NHD.csv") ### crosswalk bw NHD and DOW using spatial join in QGIS
LakeDOW.NHD

NLDAS_thermal_1995to2015=NLDAS_thermal %>% ### select only years between 1995 and 2015
filter(year >1994 & year <2016)
NLDAS_thermal_1995to2015

NLDAS_thermal_1995to2015.DOW=left_join(NLDAS_thermal_1995to2015,LakeDOW.NHD, by="site_id")
NLDAS_thermal_1995to2015.DOW

NLDAS.therm.95to15.DOW=NLDAS_thermal_1995to2015.DOW %>% ### select only few needed temp. variables
select(year, site_id, peak_temp, gdd_wtr_10c,  mean_surf_jas, max_surf_jas, DOWLKNUM)
View(NLDAS.therm.95to15.DOW)

NLDAS.therm.aggbyDOW=NLDAS.therm.95to15.DOW %>%
group_by(DOWLKNUM) %>%
summarise(
mean.peak_temp=mean(peak_temp),
mean.gdd_wtr_10c=mean(gdd_wtr_10c),
mean.mean_surf_jas=mean(mean_surf_jas),
mean.max_surf_jas=mean(max_surf_jas)
)
NLDAS.therm.aggbyDOW

NLDAS.therm.aggbyDOWYear=NLDAS.therm.95to15.DOW %>%
group_by(DOWLKNUM, year) %>%
summarise(
mean.peak_temp=mean(peak_temp),
mean.gdd_wtr_10c=mean(gdd_wtr_10c),
mean.mean_surf_jas=mean(mean_surf_jas),
mean.max_surf_jas=mean(max_surf_jas)
)
NLDAS.therm.aggbyDOWYear

write.csv(NLDAS.therm.aggbyDOW,"MinnEWMdata/NLDAS.therm9515.aggbyDOW.csv")
write.csv(NLDAS.therm.aggbyDOWYear,"MinnEWMdata/NLDAS.therm9515.aggbyDOW.YEAR.csv")

#############################################################################################################################################

LakeIndexFile=read_csv("MinnEWMdata/LakeIndexFile.csv") ##
dim(LakeIndexFile)
NLDAS.therm.aggbyDOW
NLDAS.therm.aggbyDOWYear
##### Merge the index file with NLDAS water temp. to create a seperate lake temperature file
EWMlakeindex.NLDAStherm.byDOW=left_join(LakeIndexFile, NLDAS.therm.aggbyDOW, by="DOWLKNUM")


library(skimr)
skim_without_charts(EWMlakeindex.NLDAStherm.byDOW)

### modify skimr functional list
my_skim <- skim_with(numeric = sfl(hist=NULL), base = sfl(
missing = n_missing,
complete = n_complete,
n = length
))
### Skim and summarize sepearatly by EWMStatus 
#### All unsurveyed lakes
EWMlakeindex.NLDAStherm.byDOW %>%
filter(EWMSTATUS=="U") %>%
my_skim()
##### All EWM presences
EWMlakeindex.NLDAStherm.byDOW %>%
filter(EWMSTATUS=="1") %>%
my_skim()
##### EWM absences
EWMlakeindex.NLDAStherm.byDOW %>%
filter(EWMSTATUS=="0") %>%
my_skim()


##### Merge the lake watchem data with NLDAS water temp. to create a separate lake temperature file
EWMlakeindex.allwatchembyDOW=read_csv("MinnEWMdata/EWMlakeindex.SecchiCDOM.Watchem.selvars.byDOW.csv")
EWMlakeindex.allwatchem.NLDASwtmp=left_join(EWMlakeindex.allwatchembyDOW, NLDAS.therm.aggbyDOW, by="DOWLKNUM")

write_csv(EWMlakeindex.allwatchem.NLDASwtmp, "MinnEWMdata/MergedData/EWMlakeindex.allwatchem.NLDASwtmp.csv")

my_skim(EWMlakeindex.allwatchem.NLDASwtmp)
skim_without_charts(EWMlakeindex.allwatchem.NLDASwtmp)

EWMlakeindex.allwatchem.NLDASwtmp%>%
group_by(EWMSTATUS) %>%
my_skim()


EWMlakeindex.allwatchem.NLDASwtmp %>%
filter(EWMSTATUS == "1") %>%
my_skim()

EWMlakeindex.allwatchem.NLDASwtmp %>%
  filter(EWMSTATUS == "0") %>%
  my_skim()

EWMlakeindex.allwatchem.NLDASwtmp %>%
  filter(EWMSTATUS != "U") %>%
  my_skim()

EWMlakeindex.allwatchem.NLDASwtmp %>%
  filter(EWMSTATUS == "U") %>%
  my_skim()

########################################### Some data visualization of the fully merged dataset
library(corrplot)

EWMlakeindex.allwatchem.NLDASwtmp ### the full merged dataset so far: lake watchem and water temperature

data.frame(colnames(EWMlakeindex.allwatchem.NLDASwtmp)) ## a quick look at the column names and numbers
EWM.pred.cor=round(cor(EWMlakeindex.allwatchem.NLDASwtmp[,c(3,4,15:35)], use = "pairwise.complete.obs", method = "pearson"), 2)

corrplot(EWM.pred.cor, type = "upper", order = "FPC",
tl.col = "black", tl.srt = 90, method = "circle", number.cex = 0.5, addCoef.col = "black")

### Try a PCA to see the correlations among variables 
library(factoextra)
library(ade4)
library(sp)

data.frame(colnames(EWMlakeindex.allwatchem.NLDASwtmp)) ## a quick look at the column names and numbers
EWMlake.pca.data=EWMlakeindex.allwatchem.NLDASwtmp[,c(15:35)]
EWMlake.pca.data=EWMlake.pca.data %>%
na.omit()
EWMlake.pca=dudi.pca(EWMlake.pca.data,scannf = FALSE, nf=3)

fviz_pca_var(EWMlake.pca,
col.var = "contrib", # Color by contributions to the PC
gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
repel = TRUE     # Avoid text overlapping
)
fviz_pca_biplot(EWMlake.pca, repel = TRUE,
col.var = "#2E9FDF", # Variables color
col.ind = "#696969"  # Individuals color
)





