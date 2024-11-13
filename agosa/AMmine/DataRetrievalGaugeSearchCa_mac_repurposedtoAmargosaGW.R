rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(dataRetrieval)
#library(ggridges)
library(sf)
#library(viridis)
library(rgdal)
#source("fun_defs.r")


#huc8 - upper amargosa 18090202
# huc 10 - rock valley 180902215


## get groundwater levels
huc8gwlvl_ua <- readNWISdata(huc="18090202", service="gwlevels")
head(huc8gwlvl_ua)
huc8gwlvl_la <- readNWISdata(huc="18090203", service="gwlevels")
head(huc8gwlvl_la)
huc8gwlvl_ss <- readNWISdata(huc="16060014", service="gwlevels") #sand spring- tikaboo
head(huc8gwlvl_ss)

#combine gw levels of all 3 river basins
huc8gwlvl_ua_la_ss <- rbind(huc8gwlvl_ua, huc8gwlvl_la, huc8gwlvl_ss)

#huc10gwlvl_rv <- readNWISdata(huc="1809020215", service="gwlevels") #dataRetrieval not returning for HUC10s, even though wells clearly in it
#head(huc10gwlvl_rv)

## get unique measurement sites
huc8gwlvl_ua_unqe <- unique(huc8gwlvl_ua$site_no)
huc8gwlvl_la_unqe <- unique(huc8gwlvl_la$site_no)
huc8gwlvl_ss_unqe <- unique(huc8gwlvl_ss$site_no)
#huc10gwlvl_rv_unqe <- unique(huc10gwlvl_rv$site_no)                        

## get site coordinates
huc8gwlvl_ua_unqe_siteinfo <- readNWISsite(huc8gwlvl_ua_unqe)
huc8gwlvl_la_unqe_siteinfo <- readNWISsite(huc8gwlvl_la_unqe)
huc8gwlvl_ss_unqe_siteinfo <- readNWISsite(huc8gwlvl_ss_unqe)
#huc10gwlvl_rv_unqe_siteinfo <- readNWISsite(huc10gwlvl_rv_unqe)

### combine and rbind
allAffectedEnvironHUC8GWsites <- rbind(huc8gwlvl_ua_unqe_siteinfo,huc8gwlvl_la_unqe_siteinfo,huc8gwlvl_ss_unqe_siteinfo)



# convert to spatial data frame
allAffectedEnvironHUC8GWsites_sp <- allAffectedEnvironHUC8GWsites %>% st_as_sf(coords = c("dec_long_va", "dec_lat_va")) %>% st_sf(crs = 4326)
     

# create and save esri-readable shapefile
st_write(allAffectedEnvironHUC8GWsites_sp, "allAffectedEnvironHUC8GWsites.shp")


### did the clipping in ESRI, reading in just the wells in AFFCR
usgs_affcr_gwsites <- st_read( "AFFCR_USGS_GWsites.shp") 

### filter the gw levels to the latter file, so there's only gw level data for the wells in AFFCR
usgs_affcr_gwlvls <- right_join(huc8gwlvl_ua_la_ss, usgs_affcr_gwsites)


###### summarize the highest the head has been for each site
usgs_affcr_gwlvls_nvd88_max <- usgs_affcr_gwlvls %>% filter(sl_datum_cd == "NAVD88") %>% group_by(site_no) %>% summarize(max = max(sl_lev_va))

###### summarize the shallowest the water table has been relative to ground surface
usgs_affcr_gwlvls_ftbgs_min <- usgs_affcr_gwlvls %>% filter(!is.na(lev_va)) %>% group_by(site_no) %>% summarize(min_ftbgs = min(lev_va))

### join the water depth min (shallowest) data to the well locations
usgs_affcr_gwsites_minftbgs <- inner_join(usgs_affcr_gwlvls_ftbgs_min, usgs_affcr_gwsites)

### save as shapefile to visualize in esri ecosystem
st_write(usgs_affcr_gwsites_minftbgs, "usgs_affcr_gwsites_minftbgs.shp")

bbox_activegwsites <- whatNWISdata(bBox = c( -117.048976,35.996318 ,-116.143854 , 37.240863), service  = "gwlevels", siteStatus = "active") #box coords for AFFCR
bbox_activegwsites_sp <- bbox_activegwsites %>% st_as_sf(coords = c("dec_long_va", "dec_lat_va")) %>% st_sf(crs = 4326)
st_write(bbox_activegwsites_sp, "bbox_activegwsites_sp.shp")

### debug
usgs_affcr_gwlvls_361703116215001 <- usgs_affcr_gwlvls %>% filter(site_no == 361703116215001)











#################################################################
################# gauge search spring/summer 2024
#################################################################
#pCode shortName
#00060 Discharge [ft3/s]
#00065 Gage height [ft]
#00010 Temperature [C]
#00045 Precipitation [in]
#00400 pH
#
#Commonly used USGS Stat Codes
#StatCode shortName
#00001 Maximum
#00002 Minimum
#00003 Mean
#00008 Median

#Ca_usgsgauge_strm_flow <- whatNWISsites(stateCd = "CA",
#                                        parameterCd = "00060")
#
#Ca_usgsgauge_strm_stge <- whatNWISsites(stateCd = "CA",
#                                        parameterCd = "00065")
#
#Ca_usgsgauge_prcp <- whatNWISsites(stateCd = "CA",
#                                   parameterCd = "00045")
#
## convert to spatial data frame
#Ca_usgsgauge_strm_flow <- Ca_usgsgauge_strm_flow %>% st_as_sf(coords = c("dec_long_va", "dec_lat_va")) %>% st_sf(crs = 4326)
#Ca_usgsgauge_strm_stge <- Ca_usgsgauge_strm_stge %>% st_as_sf(coords = c("dec_long_va", "dec_lat_va")) %>% st_sf(crs = 4326)
#Ca_usgsgauge_prcp <- Ca_usgsgauge_prcp %>% st_as_sf(coords = c("dec_long_va", "dec_lat_va")) %>% st_sf(crs = 4326)
#
## create and save esri-readable shapefile
#
#st_write(Ca_usgsgauge_strm_flow, "Ca_usgsgauge_strm_flow.shp")
#st_write(Ca_usgsgauge_strm_stge, "Ca_usgsgauge_strm_stge.shp")
#st_write(Ca_usgsgauge_prcp, "Ca_usgsgauge_strm_prcp.shp")
#
##writeOGR(Ca_usgsgauge_strm_flow, ".", "Ca_usgsgauge_strm_flow", driver = "ESRI Shapefile")
##writeOGR(Ca_usgsgauge_strm_stge, ".", "Ca_usgsgauge_strm_stge", driver = "ESRI Shapefile")
##writeOGR(Ca_usgsgauge_prcp, ".", "Ca_usgsgauge_prcp", driver = "ESRI Shapefile")
#