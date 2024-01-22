
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


df <- right_join(df, monsum)

df_wywm <- df %>% group_by(wm_wy) %>% summarize(wywmsum = sum(af))
meandailyflo_por <- mean(df$cfs)
mean_wy_taf <- mean(annwysum$wyaf)
median_wy_taf <- median(annwysum$wyaf)
#min_wy_taf <- annwysum %>% filter(wy != 2024) %>% min(wyaf)
min_wy_taf <- 268

ridgelinethickness <- 0.00000001
ridges <- ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfs, fill = wyaf )) + 
  geom_ridgeline_gradient(scale = .003, size = ridgelinethickness, alpha = 0.5) + #, min_height = -3500
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "acre feet     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) + %>%
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +  labs(x = NULL) + #theme_bw() +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1945, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(y = NULL) + #facet_grid(~name) +#+
  ggtitle("Amargosa River at Tecopa, USGS Station 10251300\nFull period of record (missing data shown),\nDaily mean flow occurrence and acre-ft per water year") 
#geom_vline(xintercept = 183, linetype = "dashed", color = "red") +
#  geom_vline(xintercept = 305, linetype = "dashed", color = "red")
ridges
ggsave("amargosa_tecopa_dailyflow_ridges.jpg", width = 7, height  = 9, dpi = 300, units = "in")

ggplot(annwysum %>% filter(wy!= 2024), aes(x = wy, y = wyaf, fill = wyaf)) + geom_bar(position = "dodge",stat = "identity") +
  ggtitle("Amargosa River at Tecopa, USGS Station 10251300\nFull period of record (missing data shown),\nWater Year accumulated flow volume") +
  scale_fill_viridis( name = "acre feet"  ) + 
  geom_hline(yintercept = median_wy_taf, color = "red") + 
  geom_hline(yintercept = mean_wy_taf, color = "red", linetype = "dashed") + 
  
  geom_hline(yintercept = max(annwysum$wyaf), color = "gray", linetype = "dashed") + 
  geom_hline(yintercept = min_wy_taf, color = "gray", linetype = "dashed") + 
  
  
  
  scale_y_continuous(breaks = c(0,268, 750,1000,2240, 3000,4000, 5000, 6000, 7000,8000, 9000, 10000, 10792),
                     labels = c(0,268, 750,1000,2240, 3000,4000, 5000, 6000, 7000,8000, 9000, 10000,10792)) + 
  scale_x_continuous(breaks=  c(1962, 1970, 1975, 1983, 1990, 2000, 2010, 2016, 2023),
                     labels = c(1962, 1970, 1975, 1983, 1990, 2000, 2010, 2016,  2023)) +
   labs(x = NULL, y = NULL)
ggsave("amargosa_tecopa_dailyflow_ridges.jpg", width = 7, height  = 5, dpi = 300, units = "in")


 annwysum  %>%
  ggplot(aes(x =  wyaf, y = wy, fill = wyaf, color  = wyaf, label = round(wyaf, 0))) +
  geom_bar(position = "dodge",stat = "identity") + 
  theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
  ylab("taf") + geom_text(color = "dark blue", angle = 90, hjust = 1) + 
 # scale_x_reordered() +
  #facet_wrap(~dv, nrow = 1, scales = "free_x") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_y_continuous(sec.axis = dup_axis(name = NULL) )+
  #scale_fill_manual(values=df_cols) +
  ggtitle("mean annual (82 yrs)")





# hourly
#dischargeUnit <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
#dischargeUnit <- renameNWISColumns(dischargeUnit)
