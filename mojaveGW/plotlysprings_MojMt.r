rm(list = ls())
library(tidyverse)
library(mapview)
library(sf)
library(rgdal)
library(leaflet)

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

maptypes = c(#"Stamen.TonerLite", 
  #"Stamen.Terrain", 
  "Stamen.TopOSMRelief", 
  "Esri.WorldTopoMap" , 
  "Esri.WorldPhysical", 
  "OpenTopoMap" ,
  "NASAGIBS.ModisTerraTrueColorCR")

grp <- c(  "newzones", "allzones", "p19p13_p_perdiff", 
           #"p19p13_t_perdiff",
           #"p19xlsoldercap_perdiff",
           # "p19xlsoldestcap_perdiff",
           # "p19_meanann_p_in", 
           # "square_mi" , 
           # "mean_ft", 
           
           "rfc_off_fcast_pnts",
           "rfc_pnts", 
           
           "usgs hydrography") #, "0.5 reflectivity") 

nfo <- readOGR(".", "NFOsprings") %>% st_as_sf() %>% mutate(source = "needlesfo_2021") #%>% st_collection_extract("POINT")
#sf::st_is_valid(nfo)
head(nfo)
plot(nfo)

rfo <- readOGR(".", "RIsprings") %>% st_as_sf() %>% mutate(source = "ridgecrestfo_2022")
head(rfo)
plot(rfo)

blmCa <- readOGR(".", "BLMCaLands") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")
#head(blmCa)

drecp <- readOGR(".", "DRECPsprings") %>% st_as_sf() %>% mutate(source = "data.basin_2022")
#head(drecp)

fo <- readOGR(".", "FObounds") %>% st_as_sf() %>% transmute(Office = ADMU_NAME, Dist = PARENT_NAM, geometry)
head(fo)

nhd <- readOGR(".", "nhdv2hrsprings") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")
#head(nhd)

nh <- readOGR(".", "NinaH_SoCalDesertSprings") %>% st_as_sf() %>% mutate(source = "NH_2022")
head(nh)
plot(nh)

ssi <- readOGR(".", "SSIsprings") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")
#head(SSIsprings)

usgs <- readOGR(".", "USGSCuyama") %>% st_as_sf() #%>% mutate(source = "ridgecrestfo_2022")
#head(SSIsprings)


mapviewOptions(fgb = FALSE)
mapviewOptions(basemaps = c(
 # "Stamen.TopOSMRelief", 
  #"Stamen.Terrain",
  "Esri.WorldTopoMap" , 
  "Esri.WorldImagery" , 
 # "USGS.USTopo",
  
 "NASAGIBS.ModisTerraTrueColorCR" ,
#"OpenWeatherMap.Rain" ,
 # "Esri.WorldPhysical", 
 # "OpenTopoMap" ,
#"USGS.USImageryTopo",
 # "USGS.USTopo"))
"OpenStreetMap"))
mapview(nfo) #no
mapview(rfo) #no
mapview(blmCa) #yes
mapview(drecp) #yes
mapview(fo) #yes
mapview(nhd) #no
mapview(nh) #blank
mapview(ssi) #yes
mapview(usgs) #yes

mapview(list(fo, usgs, ssi, drecp, nfo, rfo), 
        layer.name = c("fo", "usgs_cuy" ,"ssi", "drecp", "nfo", "rfo", "nhd"), 
        col.regions=list("grey","blue", "purple", "green", "brown", "navy", "orange"))
mapview(fo, zcol = "Office") + mapview(rfo, color = "red") + mapview(usgs, color = "green") 

maptypes = c(#"Stamen.TonerLite", 
  #"Stamen.Terrain", 
  "Stamen.TopOSMRelief", 
  "Esri.WorldTopoMap" , 
  "Esri.WorldPhysical", 
  "OpenTopoMap" ,
  "NASAGIBS.ModisTerraTrueColorCR",
  "USGS.USTopo")


mapshot(map, url = "springs.html")
