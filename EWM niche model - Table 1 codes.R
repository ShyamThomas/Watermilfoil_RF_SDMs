########################### DOCUMENTING CODES FOR ALL FIGURES & TABLES IN EWM NICHE MODELING MANUSCRIPT
######################## All codes written by Shyam Thomas

#### Table 1. Lets start with a summary table of data going into the SDMs

library(skimr)
library(tidyverse)

my_skim(EWM.infes_relfrq.selpreds.naomit)
EWM.skim=EWM.infes_relfrq.selpreds.naomit%>%
dplyr::group_by(EWMSTATUS_corrRelFrq)%>%
my_skim()

EWM.infes_relfrq.preds=read_csv("MinnEWMdata/EWMinfes_relfrq.preds.data.csv")
EWM.infes_relfrq.preds

selvars=c('DOWLKNUM', 'UTMX','UTMY','LON','LAT', 'EWMSTATUS','EWMSTATUS_corrRelFrq','avg_totalalkalinity','max_depth','size_acres', 
          'avg_ph','avg_secchi','avg_conductance','avg_nitrogen','avg_phosphorus', 'avg_chlorophylla', 'a440.cdom', 'mean.gdd_wtr_10c')
EWM.infes_relfrq.selpreds=EWM.infes_relfrq.preds[selvars]

EWM.infes_relfrq.selpreds.naomit=EWM.infes_relfrq.selpreds%>% ### All EWM lakes included in all SDMs... pseudoabsence lakes too
dplyr::select(-c(avg_totalalkalinity,avg_nitrogen))%>%
na.omit()

LakeIndex.ConnData = read_csv("MinnEWMdata/LakeConnectivityData.Reduced.csv") ### Bring in the lake connectivity data
LakeIndex.ConnData
colnames(LakeIndex.ConnData)
LakeConn=LakeIndex.ConnData%>%
  dplyr::select(c(DOWLKNUM, allstreams_density_mperha, roaddensity_density_mperha))
LakeConn

EWM.infes_relfrq.Conn.preds.naomit=left_join(EWM.infes_relfrq.selpreds.naomit,LakeConn, by="DOWLKNUM")%>%na.omit() ## Merge lake connectivity data
EWM.infes_relfrq.Conn.preds.naomit

sdm.data.myskim=EWM.infes_relfrq.Conn.preds.naomit%>%my_skim()
sdm.data.myskim

