########################### DOCUMENTING CODES FOR ALL FIGURES IN EWM NICHE MODELING MANUSCRIPT
######################## All codes written by Shyam Thomas

#### Figure 1. Lets start with maps of the five different EWM lake distributions
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(sf)
library(sp)
library(tidyverse)

###### Map the state of MN - the study area
Minn.sf=read_sf(dsn="/Users/thom7552/UMNpostdoc/ProjectEWM/MinnEWM/MinnGISlayers", layer="Minn.map")
plot(Minn.sf$geometry)
Minn.sf
st_crs(Minn.sf)

##### Map the observed EWM invaded and uninvaded lakes
EWM.infes_relfrq.selpreds.prsabs=read_csv("MinnEWMdata/EWM.infes_relfrq.selpreds.prsabs.csv")
EWM.infes_relfrq.selpreds.prsabs
EWM.infes_relfrq.selpreds.prsabs.sf=st_as_sf(EWM.infes_relfrq.selpreds.prsabs, coords = c("UTMX","UTMY"), crs=32615)
EWM.infes_relfrq.selpreds.prsabs.sf

EWM.prsabs=ggplot() +
geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
geom_sf(data=EWM.infes_relfrq.selpreds.prsabs.sf, cex=1, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+
scale_color_manual(values=c("blue", "red"))
EWM.prsabs+theme_gray(base_size = 22)+theme(legend.position = "none")

png("MinnEWMfigures/EWM.obs.prsabs.map.png",units="in", width=5, height=6, res=900)
EWM.prsabs+theme_gray(base_size = 22)+theme(legend.position = "none")
dev.off()

##### Map the observed EWM invaded lakes and randomly selected unsurveyed lakes as pseudoabsences
EWM.infes_relfrq.selpreds.prs.randpseuabs=read_csv("MinnEWMdata/Rand.pseuabs.sdm.csv")
EWM.infes_relfrq.selpreds.prs.randpseuabs
EWM.infes_relfrq.selpreds.prs.randpseuabs.sf=st_as_sf(EWM.infes_relfrq.selpreds.prs.randpseuabs, coords = c("UTMX","UTMY"), crs=32615)
EWM.infes_relfrq.selpreds.prs.randpseuabs.sf
  
EWM.prs.randabs=ggplot() +
  geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
  geom_sf(data=EWM.infes_relfrq.selpreds.prs.randpseuabs.sf, cex=1, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+
  scale_color_manual(values=c("blue", "red"))
EWM.prs.randabs+theme_gray(base_size = 22)+theme(legend.position = "none")

png("MinnEWMfigures/EWM.prs.randabs.map.png",units="in", width=5, height=6, res=900)
EWM.prs.randabs+theme_gray(base_size = 22)+theme(legend.position = "none")
dev.off()

##### Map the observed EWM invaded lakes and selected distant  unsurveyed lakes as pseudoabsences
EWM.infes_relfrq.selpreds.prs.outrand.pseuabs=read_csv("MinnEWMdata/OutRand.pseuabs.sdm.csv")
EWM.infes_relfrq.selpreds.prs.outrand.pseuabs
EWM.infes_relfrq.selpreds.prs.outrand.pseuabs.sf=st_as_sf(EWM.infes_relfrq.selpreds.prs.outrand.pseuabs, coords = c("UTMX","UTMY"), crs=32615)
EWM.infes_relfrq.selpreds.prs.outrand.pseuabs.sf

EWM.prs.outrandabs=ggplot() +
geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
geom_sf(data=EWM.infes_relfrq.selpreds.prs.outrand.pseuabs.sf, cex=1, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+
scale_color_manual(values=c("blue", "red"))
EWM.prs.outrandabs+theme_gray(base_size = 22)+theme(legend.position = "none")

png("MinnEWMfigures/EWM.prs.outrandabs.map.png",units="in", width=5, height=6, res=900)
EWM.prs.outrandabs+theme_gray(base_size = 22)+theme(legend.position = "none")
dev.off()

##### Map the observed EWM invaded lakes and selected proximal  unsurveyed lakes as pseudoabsences
EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs=read_csv("MinnEWMdata/NearRand.pseuabs.sdm.csv")
EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs
EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs.sf=st_as_sf(EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs, coords = c("UTMX","UTMY"), crs=32615)
EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs.sf

EWM.prs.nearrandabs=ggplot() +
  geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
  geom_sf(data=EWM.infes_relfrq.selpreds.prs.nearrand.pseuabs.sf, cex=1, aes(colour=as.factor(EWMSTATUS_corrRelFrq)))+
  scale_color_manual(values=c("blue", "red"))
EWM.prs.nearrandabs+theme_gray(base_size = 22)+theme(legend.position = "none")

png("MinnEWMfigures/EWM.prs.nearrandabs.map.png",units="in", width=5, height=6, res=900)
EWM.prs.nearrandabs+theme_gray(base_size = 22)+theme(legend.position = "none")
dev.off()

### Extract the legend for all maps
library(ggpubr)

prsabs.legend=get_legend(EWM.prs.nearrandabs+theme_bw(base_size = 18)+theme(legend.title = element_blank()))
plot(prsabs.legend)
png("MinnEWMfigures/EWM.prs.abs.legend.png",units="in", width=5, height=6, res=900)
plot(prsabs.legend)

#################################################################################################################################################
##### Map abundance data of EWM invasion
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

EWM.relfrq.Conn.preds.data.selvars=EWM.relfrq.Conn.preds.data%>%
  dplyr::select(-c(avg_totalalkalinity, avg_nitrogen,  DOWLKNUM, UTMX, UTMY))%>%
  na.omit()
EWM.relfrq.Conn.preds.data.selvars ### Data with EWM relative frequency measure of abundance
SDM.relfrq.data.sf=st_as_sf(EWM.relfrq.Conn.preds.data.selvars,coords=c("LON","LAT"),crs=st_crs(Minn.sf))
SDM.relfrq.data.sf

EWM.abun.map=ggplot()+
geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
geom_sf(data=SDM.relfrq.data.sf$geometry, alpha=0.5, aes(color=SDM.relfrq.data.sf$MYS_relfrq))+
scale_color_gradientn(colours = topo.colors(20))+ theme(legend.title = element_blank())
EWM.abun.map+theme_gray(base_size = 18)+ theme(legend.title = element_blank())

png("MinnEWMfigures/EWM.abun.map.png",units="in", width=5, height=6, res=900)
EWM.abun.map+theme_gray(base_size = 18)+ theme(legend.title = element_blank())
dev.off()


