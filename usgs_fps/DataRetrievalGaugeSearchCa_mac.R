rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(dataRetrieval)
library(ggridges)
library(sf)
library(viridis)
library(rgdal)
#source("fun_defs.r")

#pCode shortName
#00060 Discharge [ft3/s]
#00065 Gage height [ft]
#00010 Temperature [C]
#00045 Precipitation [in]
#00400 pH

#Commonly used USGS Stat Codes
#StatCode shortName
#00001 Maximum
#00002 Minimum
#00003 Mean
#00008 Median

Ca_usgsgauge_strm_flow <- whatNWISsites(stateCd = "CA",
                                        parameterCd = "00060")

Ca_usgsgauge_strm_stge <- whatNWISsites(stateCd = "CA",
                                        parameterCd = "00065")

Ca_usgsgauge_prcp <- whatNWISsites(stateCd = "CA",
                                   parameterCd = "00045")

# convert to spatial data frame
Ca_usgsgauge_strm_flow <- Ca_usgsgauge_strm_flow %>% st_as_sf(coords = c("dec_long_va", "dec_lat_va")) %>% st_sf(crs = 4326)
Ca_usgsgauge_strm_stge <- Ca_usgsgauge_strm_stge %>% st_as_sf(coords = c("dec_long_va", "dec_lat_va")) %>% st_sf(crs = 4326)
Ca_usgsgauge_prcp <- Ca_usgsgauge_prcp %>% st_as_sf(coords = c("dec_long_va", "dec_lat_va")) %>% st_sf(crs = 4326)

# create and save esri-readable shapefile

st_write(Ca_usgsgauge_strm_flow, "Ca_usgsgauge_strm_flow.shp")
st_write(Ca_usgsgauge_strm_stge, "Ca_usgsgauge_strm_stge.shp")
st_write(Ca_usgsgauge_prcp, "Ca_usgsgauge_strm_prcp.shp")

#writeOGR(Ca_usgsgauge_strm_flow, ".", "Ca_usgsgauge_strm_flow", driver = "ESRI Shapefile")
#writeOGR(Ca_usgsgauge_strm_stge, ".", "Ca_usgsgauge_strm_stge", driver = "ESRI Shapefile")
#writeOGR(Ca_usgsgauge_prcp, ".", "Ca_usgsgauge_prcp", driver = "ESRI Shapefile")
