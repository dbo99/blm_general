rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(mapview)
library(sf)
#library(rgdal)
library(leaflet)

#BLMCa_USGSFPS_23May2024
#BLMCaACECs_23May2024
#BLMCaLands_23May2024

####### read in shapefiles 
BLMCa <- st_read("BLMCaLands_23May2024_dsvld.shp")
BLM_FOouter <- st_read("BLMCa_FOouter.shp") %>% transmute(office = ADMU_NAME, district = PARENT_NAM)
huc8 <- st_read("huc8_Ca.shp") %>% transmute(huc8, name)
BLMCa_FPSReprioRequest <- st_read("BLMCa_USGSFPS_23May2024.shp") #%>% select(-Field)
head(BLMCa_FPSReprioRequest)
BLMCa_FPSReprioRequest <- BLMCa_FPSReprioRequest %>% transmute(site_no, station_nm, typ, lat_nad83, lon_nad83, huc8, 
                                                               huc8nm,blm_fo = blm_fieldo, prev_establshd = previously,
                                    BLM_POC, OnBLMLands = OnBLMLand_, BLMFPSPrio = Submittedb, geometry)
BLMCa_FPSReprioRequest_new <- BLMCa_FPSReprioRequest %>% filter(prev_establshd == "No")
head(BLMCa_FPSReprioRequest)
BLMCa_NatlMnmnt <- st_read("BLMCaNatlMnmt_23May2024.shp") %>% transmute(NLCS_NAME)
BLMCa_WSR <- st_read("BLMCaWSRs_23May2024.shp") %>% transmute(NLCS_NAME, ADMIN_AGEN, CATEGORY_c)
BLMCa_WSRCorr <- st_read("BLMCaWSRCorridor_23May2024.shp") %>% transmute(NLCS_NAME, WSR_CTGY)
BLMCa_ACECs <- st_read("BLMCaACECs_23May2024.shp") %>% transmute(ACEC_NAME, LUP_NAME)
BLMCa_wild <- st_read("BLMCaWilderness_23May2024.shp") %>% transmute(NLCS_NAME, DESIG_DATE)
BLMCa_springs_all <- st_read("BLMCaSpringsAll_23May2024.shp") %>% transmute(name, source, lat_nad83 = POINT_Y, lon_nad83 = POINT_X)
BLMCa_springs_wild <- st_read("BLMCaSpringsInWilderness_23May2024.shp")# %>% transmute(NLCS_NAME, DESIG_DATE)
CaPrinAquifer <- st_read("bull118basins2021.shp") %>% transmute(name = Basin_Su_1)


#head(SSIsprings)


mapviewOptions(fgb = FALSE) #workaround for mapview inexplicably not plotting some shapefiles
#mapviewOptions(hide = TRUE)
mapviewOptions(basemaps = c(
 # "Stamen.TopOSMRelief", 
  #"Stamen.Terrain",
  "USGS.USTopo",
  "OpenStreetMap",
  "Esri.WorldTopoMap" , 
  "Esri.WorldImagery"  
  ))
  
# "NASAGIBS.ModisTerraTrueColorCR" ))


 m <- 
   mapview(BLMCa_FPSReprioRequest, zcol = "station_nm", col.region = "blue", legend = FALSE, cex = 5) +
   mapview(BLMCa_FPSReprioRequest_new, zcol = "station_nm", color = "red", legend = FALSE, cex = 7, alpha.regions = 0.0) +
   mapview(BLMCa_springs_wild, zcol = "name", col.region = NA, legend = FALSE, hide = TRUE, cex = 2.5) +
   mapview(BLMCa_springs_all, zcol = "name", col.region = NA, legend = FALSE, hide = TRUE, cex = 3) +

   
   mapview(BLMCa, col.region = "yellow", legend = FALSE, hide = TRUE) +
   mapview(huc8, zcol = "name", col.region = NA, alpha = 0.9, legend = FALSE, hide = TRUE, cex = 2) +

   mapview(BLMCa_ACECs, zcol = "ACEC_NAME", col.region = "orange", legend = FALSE, hide = TRUE) +

   mapview(BLMCa_WSR, zcol = "NLCS_NAME", color = "blue", legend = FALSE, hide = TRUE) +
   mapview(BLMCa_WSRCorr, zcol = "NLCS_NAME", color = "pink", legend = FALSE, hide = TRUE) +
   mapview(BLMCa_wild, zcol = "NLCS_NAME",  legend = FALSE, hide = TRUE) +

   mapview(BLMCa_NatlMnmnt, zcol = "NLCS_NAME", color = "gray", legend = FALSE, hide = TRUE) +
   mapview(CaPrinAquifer, zcol = "name", col.region = NA, legend = FALSE, hide = TRUE) +
   mapview(BLM_FOouter, zcol = "office", col.region = NA, legend = FALSE, hide = TRUE) 
 
 
   
   
   
 m@map %>% setView(-121.5, 38.52, zoom = 6) #%>% mapshot(url = "fps.html")




#mapshot(m, url = "springs.html")

#df_sprngs <- rbind(rfo_yellow, mastsurvtable_purple, drecp_brown, ssi_darkblue, nhd_orange) %>% st_as_sf()
#st_crs(df_sprngs) = 4326
#st_crs(df_sprngs$geometry) <- 4326
 #ggplot() + geom_sf(data = df_sprngs) + facet_wrap(~source)
 
 
 #m@map %>% setView(-115.40382, 34.58, zoom = 9) %>% mapshot(url = "CaDWRCompletionReports.html")
 
 #
 #mapview(list(fo, usgs, ssi, drecp, nfo, rfo, nhd, blmCa), 
 #        layer.name = c("fo", "usgs_cuy" ,"ssi", "drecp", "nfo", "rfo", "nhd", "blmCa"), 
 #        col.regions=list("grey","blue", "purple", "green", "brown", "navy", "orange", "light gray"))
 #mapview(fo, zcol = "Office") + mapview(rfo, color = "red") + mapview(usgs, color = "green") 
 
 
 
 
 mapshot(m, url = "springs.html")
 
 