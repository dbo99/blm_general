


{
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
           
           "usgs hydrography") #, "0.5 reflectivity") }
}


#{
blmca <- readOGR(".", "blmca_lands_wsonly") %>% st_as_sf() #%>% transmute(Obj = OBJECTID)
#sf::st_is_valid(nfo)
#head(blmca)
#plot(blmca)

usfslands <- readOGR(".", "usfs_lands_wsonly") %>% st_as_sf() #%>% transmute(Obj = OBJECTID)
#head(usfslands)
#plot(usfslands)


swrcb_l<- readOGR(".", "swrcb_lstd_wsonly") %>% st_as_sf() %>% transmute(Obj = objectid, name_swrcb = waterbody_)
#head(swrcb_nl)

swrcb_nl <- readOGR(".", "swrcb_nl_wsonly") %>% st_as_sf() %>% transmute(Obj = objectid, name_swrcb = waterbody_)
#head(swrcb_nl)

nhd <- readOGR(".", "nhd") %>% st_as_sf()  #%>% transmute( Permanent_)
#head(nhd)

huc12_basinsofinterest <- readOGR(".", "huc12") %>% st_as_sf()  %>% transmute( Name)

huc14 <- readOGR(".", "huc14") %>% st_as_sf()# %>% transmute(Name)

huc12_panoche <-  readOGR(".", "panoche_huc12") %>% st_as_sf() %>% transmute(Name)
huc12_humbug <-  readOGR(".", "humbug_huc12") %>% st_as_sf()  %>% transmute(Name)
huc12_fall <-  readOGR(".", "fallriver_huc12") %>% st_as_sf() %>% transmute(Name) %>%
                filter(Name != "Whitehorse Flat Reservoir")
#}

mapviewOptions(fgb = FALSE)
mapviewOptions(basemaps = c(
  # "Stamen.TopOSMRelief", 
  "Stamen.Terrain",
  "Esri.WorldTopoMap" , 
  "Esri.WorldImagery" , 
  #"USGS.USTopo",
  
  "NASAGIBS.ModisTerraTrueColorCR" ,
  #"OpenWeatherMap.Rain" ,
  # "Esri.WorldPhysical", 
  # "OpenTopoMap" ,
  #"USGS.USImageryTopo",
  # "USGS.USTopo"))
  "OpenStreetMap"))

#mapview(huc12, alpha = 0.05, alpha.regions = 0.3, legend = FALSE) #yes
#mapview(blmca) #no
#mapview(usfslands) #no
#mapview(swrcb_nl) #yes
#mapview(swrcb_l) #yes
#mapview(nhd) #yes
#mapview(huc14) #blank
#mapview(huc12_fall)
#mapview(huc12_panoche)
#mapview(huc12_humbug)



map <- mapview(list(huc12_basinsofinterest, blmca, usfslands, swrcb_nl, swrcb_l, nhd, huc14, huc12_fall,
             huc12_humbug, huc12_panoche), 
        layer.name = c("huc12_basinsofinterest",  "blm", "usfs" ,"swrcb_notlisted", "swrcb_listed", "nhd_fall_hmbg_pnche", "huc14_justhumbug", 
                       "huc12_fallriver", "huc12_humbug", "huc12_panoche"), 
       
        legend = rep("False", 10),
        col.regions= c("maroon", "yellow","green", "dark blue", "red", "blue", "pink",
                       "purple", "purple", "purple"),
        color= c("maroon",  "yellow","green", "dark blue", "red", "blue", "pink",
                 "purple", "purple", "purple"),
        lwd = c(1,1,1,1,1,1,1,1,1,1),
      #  col.regions= c("yellow","green", "blue", "red", "blue", "pink", "purple"),
        alpha.regions= c(0.3, 0.3,0.3, 0.3, 0.3, 0.3, 0.3, 0.12, 0.12, 0.12), hide = TRUE)

#mapview(fo, zcol = "Office") + mapview(rfo, color = "red") + mapview(usgs, color = "green") 

#aptypes = c(#"Stamen.TonerLite", 
# #"Stamen.Terrain", 
# "Stamen.TopOSMRelief", 
# "Esri.WorldTopoMap" , 
# "Esri.WorldPhysical", 
# "OpenTopoMap" ,
# "NASAGIBS.ModisTerraTrueColorCR",
# "USGS.USTopo")


mapshot(map, url = "swrcb303sed_blmca_usfs.html")
