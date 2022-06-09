rm(list = ls())
library(tidyverse)
library(mapview)
library(sf)
library(rgdal)
library(leaflet)

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####### read in shapefiles 

nfo_green <- readOGR(".", "NFOsprings") %>% st_as_sf() %>% mutate(source = "needlesfo_2021") #%>% st_collection_extract("POINT")
#sf::st_is_valid(nfo)
#head(nfo)
#plot(nfo)

usgs_cuyama <- readOGR(".", "USGSCuyama") %>% st_as_sf() %>% mutate(source = "needlesfo_2021") #%>% st_collection_extract("POINT")


rfo_yellow <- readOGR(".", "RIsprings") %>% st_as_sf() %>% mutate(source = "ridgecrestfo_2022")
#head(rfo)
#plot(rfo)

blmCa <- readOGR(".", "BLMCaLands") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")
head(blmCa)

drecp_brown <- readOGR(".", "DRECPsprings") %>% st_as_sf() %>% mutate(source = "data.basin_2022")
#head(drecp)

fieldofficeouterbounds <- readOGR(".", "FObounds") %>% st_as_sf() %>% transmute(Office = ADMU_NAME, Dist = PARENT_NAM, geometry)
head(fo)

nhd_orange <- readOGR(".", "nhdv2hrsprings") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")
#head(nhd)

mastsurvtable_purple <- readOGR(".", "NinaH_SoCalDesertSprings_mastersurveytable") %>% st_as_sf() %>% mutate(source = "NH_2022")
head(nh)
plot(nh)

ssi_darkblue <- readOGR(".", "SSIsprings") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")
#head(SSIsprings)

gw_basin <- readOGR(".", "b118_wgs84") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")
#head(SSIsprings)

monuments <- readOGR(".", "Monuments_wgs84") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")
#head(SSIsprings)

wellcompreports <- readOGR(".", "DWRwells_VerySECali") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")

mapviewOptions(fgb = FALSE) #workaround for mapview inexplicably not plotting some shapefiles
#mapviewOptions(hide = TRUE)
mapviewOptions(basemaps = c(
 # "Stamen.TopOSMRelief", 
  #"Stamen.Terrain",
  "OpenStreetMap",
  "Esri.WorldTopoMap" , 
  "Esri.WorldImagery" , 
 # "USGS.USTopo",
  
 "NASAGIBS.ModisTerraTrueColorCR" ))
#"OpenWeatherMap.Rain" ,
 # "Esri.WorldPhysical", 
 # "OpenTopoMap" ,
#"USGS.USImageryTopo",
 # "USGS.USTopo"))

#mapview(nfo) #no
#mapview(rfo) #no
#mapview(blmCa) #yes
#mapview(drecp) #yes
#mapview(fo) #yes
#mapview(nhd) #no
#mapview(nh) #blank
#mapview(ssi) #yes
#mapview(usgs) #yes

#mapview(list(fo, usgs, ssi), 
#       layer.name = c("fo", "usgs_cuy" ,"ssi"), 
#       col.regions=list("grey","blue", "purple"))


 m <- mapview(fieldofficeouterbounds, zcol = "Office", col.region = "grey", legend = FALSE, hide = TRUE) +
   
  mapview(wellcompreports, zcol = "WCRNumber", cex = "TotalCompl", col.region = "blue", legend = FALSE, hide = TRUE) +
 # mapview(nfo_green, zcol = "name", col.region = "light green", legend = FALSE, hide = TRUE, cex = 2) +
 # mapview(rfo_yellow, zcol = "name", col.region = "yellow", legend = FALSE, hide = TRUE, cex = 2) +
 # mapview(mastsurvtable_purple, zcol = "name", col.region = "purple", legend = FALSE, hide = TRUE, cex = 3) + 
 # mapview(drecp_brown, zcol = "name", col.region = "brown", legend = FALSE, hide = TRUE, cex =4) +
 # mapview(ssi_darkblue, zcol = "name", col.region = "navy", legend = FALSE, hide = TRUE, cex = 5) +
 # mapview(nhd_orange, zcol = "name", col.region = "orange", legend = FALSE, hide = TRUE, cex = 6, alpha = 0.5) +
 # mapview(monuments, zcol = "Label", col.region = NA, color = "red",  legend = FALSE) +
  mapview(gw_basin, zcol = "Basin_Su_1", col.region = "light blue", legend = FALSE, hide = TRUE) +
  mapview(blmCa, zcol = "ADMU_NAME",  legend = FALSE, hide = TRUE) 
   
   
   
 m@map %>% setView(-115.40382, 34.58, zoom = 9) %>% mapshot(url = "CaDWRCompletionReports.html")

   #
#mapview(list(fo, usgs, ssi, drecp, nfo, rfo, nhd, blmCa), 
#        layer.name = c("fo", "usgs_cuy" ,"ssi", "drecp", "nfo", "rfo", "nhd", "blmCa"), 
#        col.regions=list("grey","blue", "purple", "green", "brown", "navy", "orange", "light gray"))
#mapview(fo, zcol = "Office") + mapview(rfo, color = "red") + mapview(usgs, color = "green") 




mapshot(m, url = "springs.html")

df_sprngs <- rbind(rfo_yellow, mastsurvtable_purple, drecp_brown, ssi_darkblue, nhd_orange) %>% st_as_sf()
st_crs(df_sprngs) = 4326
#st_crs(df_sprngs$geometry) <- 4326
 ggplot() + geom_sf(data = df_sprngs) + facet_wrap(~source)
 
 

 
 