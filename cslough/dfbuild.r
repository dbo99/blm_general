
rm(list = ls())

{

  library(tidyverse)
  library(lubridate)
  library(ggridges)
  library(viridis)
  library(zoo)
  library(plotly)
  rstudioapi::getActiveDocumentContext
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }

  #source("fun_defs.r")
  #daily_dowy <- read_csv("daily_dowy.csv")  


{
piezpos <- read_csv("piezpos.csv")
head(piezpos)
cs1 <- read_csv("r1.csv")     %>% mutate(piezo_nn = "cs1")
cs2 <- read_csv("r2.csv")     %>% mutate(piezo_nn = "cs2")
cs3 <- read_csv("r3.csv")     %>% mutate(piezo_nn = "cs3")
cs4 <- read_csv("r4.csv")     %>% mutate(piezo_nn = "cs4")
cs5 <- read_csv("r5.csv")     %>% mutate(piezo_nn = "cs5")
cs6 <- read_csv("r6.csv")     %>% mutate(piezo_nn = "cs6")
cs7 <- read_csv("r7.csv")     %>% mutate(piezo_nn = "cs7")
cs8 <- read_csv("r8.csv")     %>% mutate(piezo_nn = "cs8")
cs9 <- read_csv("r9.csv")     %>% mutate(piezo_nn = "cs9")
cs10 <- read_csv("r10.csv")   %>% mutate(piezo_nn = "cs10")
cs11 <- read_csv("r11.csv")   %>% mutate(piezo_nn = "cs11")
csdvj <- read_csv("rDVJ.csv") %>% mutate(piezo_nn = "csdvj")

pz_ts <- rbind(cs1, cs2, cs3, cs4, cs5, cs6, cs7, cs8, cs9, cs10, cs11,  csdvj)
head(pz_ts)
}
rm(list = ls()[grep("^cs", ls())]) #beware removes variables beginning with cs
head(pz_ts)  

pz_ts <- pz_ts %>% mutate(date = mdy(date), date_tm = paste0(date, " ", time)) 
head(pz_ts)
pz_ts <- pz_ts %>% mutate(date_tm = ymd_hms(date_tm))
head(pz_ts)
