#### LAGOS database explored and wrangled to capture potential landscape drivers and connectivity

library(tidyverse)
LAGOS.lakeindex=read_csv("MinnEWMdata/LAGOSNE_lakeslocus101.csv") ### a key file that identifies all the lakes with nhdid and unique lagos lake id
Lagos.Minn_lakeindex=LAGOS.lakeindex%>%
filter(state_zoneid=="State_14")         ### State_14 refers to Minnesota; extract lakes within MN
Lagos.Minn_lakeindex                     ### A Minnesota-specific LAGOS index file

#### Let's try mapping this...  for a quick visual confirmation
Minn.sf=read_sf(dsn="/Users/thom7552/UMNpostdoc/ProjectEWM/MinnEWM/MinnGISlayers", layer="Minn.map")
plot(Minn.sf$geometry)
Lagos.Minn_lakeindex.sf=st_as_sf(Lagos.Minn_lakeindex,coords=c("nhd_long", "nhd_lat"),crs=st_crs(Minn.sf))
Lagos.Minn_lakeindex.sf

MinnLagos.lakes.map=ggplot()+
geom_sf(data=Minn.sf$geometry, colour="black", fill="grey80")+
geom_sf(data=Lagos.Minn_lakeindex.sf$geometry, color="black", pch=1)
MinnLagos.lakes.map


#### Link LAGOS index data with the NDHDID-DOWLKNUM crosswalk file 
dow_nhdhr.xwalk=read_csv("MinnEWMdata/dow_nhdhr_parsed.csv") ### NHDid and DOWLKNUM crosswalk file
dow_nhdhr.xwalk
Lagos.Minn_lakeindex_DOWLKNNUM=left_join(Lagos.Minn_lakeindex,dow_nhdhr.xwalk, by="nhdid")
Lagos.Minn_lakeindex_DOWLKNNUM 
Lagos.Minn_lakeindex_DOWLKNNUM[,c(1:3,18:21)]


######### Link aquatic connectivity within a 500 m buffer around lakes with new Lagos lake index file with DOWLKNUM
LAGOS.buffer500m.conn=read_csv("MinnEWMdata/LAGOSNE_buffer500m_conn105.csv")
LAGOS.buffer500m.conn ### This includes lakes outside MN
LAGOS.buffer500m.conn_DOWLKNUM=left_join(Lagos.Minn_lakeindex_DOWLKNNUM, LAGOS.buffer500m.conn, by="lagoslakeid")
LAGOS.buffer500m.conn_DOWLKNUM%>%
filter(!is.na(DOWLKNUM))%>%
View()

LAGOS.buffer500m.conn_DOWLKNUM.narmd=LAGOS.buffer500m.conn_DOWLKNUM%>%
filter(!is.na(DOWLKNUM))

write_csv(LAGOS.buffer500m.conn_DOWLKNUM.narmd, "MinnEWMdata/LAGOS.Buffer500m.Conn_DOWLKNUM.narmd.csv") ### Aquatic connectivity data is ready!


######### Link road density  within a 500 m buffer around lakes with new Lagos lake index file with DOWLKNUM
LAGOS.buffer500m.lulc=read_csv("MinnEWMdata/LAGOSNE_buffer500m_lulc105.csv")
LAGOS.buffer500m.lulc ### a very large database made of several lulc variables including road density

LAGOS.buffer500m.roads=LAGOS.buffer500m.lulc%>%
select( buffer500m_nhdid, buffer500m_roaddensity_sum_lengthm, buffer500m_roaddensity_density_mperha,lagoslakeid)
LAGOS.buffer500m.roads  ### extract the road density measures

LAGOS.buffer500m.roads_DOWLKNUM=left_join(Lagos.Minn_lakeindex_DOWLKNNUM, LAGOS.buffer500m.roads, by="lagoslakeid")
LAGOS.buffer500m.roads_DOWLKNUM%>%
filter(!is.na(DOWLKNUM))%>%
View()

LAGOS.buffer500m.roads_DOWLKNUM.narmd=LAGOS.buffer500m.roads_DOWLKNUM%>%
filter(!is.na(DOWLKNUM))
LAGOS.buffer500m.roads_DOWLKNUM.narmd
write_csv(LAGOS.buffer500m.roads_DOWLKNUM.narmd, "MinnEWMdata/LAGOS.Buffer500m.Road_DOWLKNUM.narmd.csv") ### Road density data is ready!

##### Final merging of connectivity data with Lake index file; first with connectivity data and then with roads data
LakeIndex=LakeIndexFile[,c(1:5,13,15)]
LakeIndex
LakeIndex_LAGOS.conn500m=left_join(LakeIndex,LAGOS.buffer500m.conn_DOWLKNUM.narmd, by="DOWLKNUM")
LakeIndex_LAGOS.conn500m
View(LakeIndex_LAGOS.conn500m)

### Now link roads data
LAGOS.buffer500m.roads_DOWLKNUM.narmd
colnames(LAGOS.buffer500m.roads_DOWLKNUM.narmd)
LAGOS.selvars=c("DOWLKNUM", "buffer500m_nhdid", "buffer500m_roaddensity_sum_lengthm", "buffer500m_roaddensity_density_mperha")
LAGOS.buffer500m.roads_DOWLKNUM.narmd.selvars=LAGOS.buffer500m.roads_DOWLKNUM.narmd[LAGOS.selvars] ### too many repeating columns from connectivity
                                                                                                   ### data, so select only the most needed 
LAGOS.buffer500m.roads_DOWLKNUM.narmd.selvars
LakeIndex_LAGOS.conn.roads500m=left_join(LakeIndex_LAGOS.conn500m,LAGOS.buffer500m.roads_DOWLKNUM.narmd.selvars, by="DOWLKNUM")
LakeIndex_LAGOS.conn.roads500m
write.csv(LakeIndex_LAGOS.conn.roads500m, "MinnEWMdata/LakeConnectivityData.csv") ### the final connectivity/roads data linked to lake data




