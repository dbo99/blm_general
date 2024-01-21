
rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(dataRetrieval)
library(ggridges)
library(viridis)
tecopaflow <- c(10251300)

# Amargosa, Tecopa:
siteNumber <- "10251300"
dailydataavailable <- whatNWISdata(
  siteNumber = siteNumber,
  service = "dv",
  statCd = "00003")

parameterCd <- "00060" # Discharge
startdate <- dailydataavailable$begin_date
enddate <- dailydataavailable$end_date
source("create_dowy.r")
dowy <- read_csv("daily_dowy.csv")
dailyflo <- readNWISdv(siteNumber, parameterCd, startdate, enddate)
head(dailyflo)
colnames(dailyflo)
dailyflo <-dailyflo %>% transmute(site_no, date = Date, cfs = X_00060_00003, af = cfs*1.98347)
head(dailyflo)
dailyflo <- inner_join(dailyflo, dowy)
head(dailyflo)

source("fun_defs.r")

dailyflo <- dailyflo %>% mutate(wy = water_year(date), wm = water_month(date), month = month(date))

annwysum <- dailyflo %>% group_by(wy) %>% summarize(wyaf = sum(af))
head(annwysum)
monsum <- dailyflo %>% group_by(wy, wm) %>% summarize(wywmaf = sum(af)) %>% mutate(wm_wy = paste0(wm,"_", wy))
head(monsum)


df <- right_join(dailyflo, annwysum) %>% mutate(wm_wy = paste0(wm,"_", wy))


df <- right_join(dailyflo, monsum)

df_wywm <- df %>% group_by(wm_wy) %>% summarize(wywmsum = sum(af))

ridgelinethickness <- 0.00000001
ridges <- ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfs, fill = wyaf )) + 
  geom_ridgeline_gradient(scale = .005, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "af     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) + %>%
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1945, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL) + #facet_grid(~name) +#+
  ggtitle("Full natural flow (FNF) *annual* inflow volume (forecast volume used\n for WY 2022)") 
#geom_vline(xintercept = 183, linetype = "dashed", color = "red") +
#  geom_vline(xintercept = 305, linetype = "dashed", color = "red")
ridges

monthtiles <- ggplot(monsum, aes(wy, wm, fill = wywmaf )) + geom_tile() +scale_fill_viridis()
monthtiles



# hourly
#dischargeUnit <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
#dischargeUnit <- renameNWISColumns(dischargeUnit)
