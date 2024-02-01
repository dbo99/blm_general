


{

rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(dataRetrieval)
library(ggridges)
library(viridis)
source("fun_defs.r")
## USGS streamguage 10251300 amargosa at tecopa 
# USGS streamgage 10251330 amargosa above china wash
#USGS streamgage 10251335 willow spring by willow ck
#USGS streamgage 10251290 borehole spring
#USGSstreamgage 10251375 agosa at dumont dunes
site_no <- c(10251300, 10251330, 10251335, 10251290, 10251375)
siteName <- c("amargosa riv at tecopa", "amargosa riv abv china wash", "willow ck (spring)", 
              "borehole spring", "amargosa riv at dmnt dunes")
usgs_numbname <- data.frame(site_no, siteName)

## amargosa at tecopa
siteNumber <- site_no[1]
dailydataavailable <- whatNWISdata(
  siteNumber = siteNumber,
  service = "dv",
  statCd = "00003")

parameterCd <- "00060" # Discharge
startdate <- dailydataavailable$begin_date
enddate <- dailydataavailable$end_date
dailyflo <- readNWISdv(siteNumber, parameterCd, startdate, enddate)

amargosa_at_tecopa <- dailyflo

## amargosa above china wash
siteNumber <- site_no[2]
dailydataavailable <- whatNWISdata(
  siteNumber = siteNumber,
  service = "dv",
  statCd = "00003")

parameterCd <- "00060" # Discharge
startdate <- dailydataavailable$begin_date
enddate <- dailydataavailable$end_date
dailyflo <- readNWISdv(siteNumber, parameterCd, startdate, enddate)

amargosa_abv_chinawash <- dailyflo

## willow spring (creek)
siteNumber <- site_no[3]
dailydataavailable <- whatNWISdata(
  siteNumber = siteNumber,
  service = "dv",
  statCd = "00003")

parameterCd <- "00060" # Discharge
startdate <- dailydataavailable$begin_date
enddate <- dailydataavailable$end_date
dailyflo <- readNWISdv(siteNumber, parameterCd, startdate, enddate)

willow_spring <- dailyflo


## borehole spring (creek)
siteNumber <- site_no[4]
dailydataavailable <- whatNWISdata(
  siteNumber = siteNumber,
  service = "dv",
  statCd = "00003")

parameterCd <- "00060" # Discharge
startdate <- dailydataavailable$begin_date
enddate <- dailydataavailable$end_date
dailyflo <- readNWISdv(siteNumber, parameterCd, startdate, enddate)

borehole_spring <- dailyflo

## amargosa river at dumont dunes
siteNumber <- site_no[5]
dailydataavailable <- whatNWISdata(
  siteNumber = siteNumber,
  service = "dv",
  statCd = "00003")

parameterCd <- "00060" # Discharge
startdate <- dailydataavailable$begin_date
enddate <- dailydataavailable$end_date
#source("create_dowy.r")
#dowy <- read_csv("daily_dowy.csv")
dailyflo <- readNWISdv(siteNumber, parameterCd, startdate, enddate)

amargosa_riv_dmntdunes <- dailyflo

dailyflo <- rbind(amargosa_at_tecopa, amargosa_abv_chinawash, borehole_spring, willow_spring, amargosa_riv_dmntdunes)
rm(amargosa_at_tecopa, amargosa_abv_chinawash, borehole_spring, willow_spring, amargosa_riv_dmntdunes)


#{

head(dailyflo)
colnames(dailyflo)
dailyflo <-dailyflo %>% transmute(site_no, date = Date, cfs = X_00060_00003, af = cfs*1.98347)
head(dailyflo)


dailyflo <- dailyflo %>% mutate(wy = water_year(date), wm = water_month(date), month = month(date))


df <- dailyflo

  


source("create_dowy.r")
dowy <- read_csv("daily_dowy.csv")
dowy <- data.frame(dowy) %>% mutate(date = ymd(date))
df <- inner_join(df, dowy, by = "date")
head(df)
df_annsum <- dailyflo %>% group_by(wy, site_no) %>% summarize(wyaf = sum(af))
head(df_annsum)
df_monsum <- dailyflo %>% group_by(wy, wm, site_no) %>% summarize(wywmaf = sum(af)) %>% mutate(wm_wy = paste0(wm,"_", wy))
head(df_monsum)

df <- right_join(df, df_annsum) 
df <- right_join(df, df_monsum)  

rm(dailydataavailable, dailyflo, df_cumdoy, dowy)


}


df <- df %>% mutate(site_no = as.integer(site_no))
df_annsum <- df_annsum %>% mutate(site_no = as.integer(site_no)) 
df_monsum <- df_monsum %>% mutate(site_no = as.integer(site_no))
df <- right_join(df, usgs_numbname)
df_annsum <- right_join(df_annsum, usgs_numbname)
df_monsum <- right_join(df_monsum, usgs_numbname)

ggplot(df_monsum, aes(wm,wywmaf, color = wy, group = wy)) +
  geom_line()   + scale_y_continuous(trans='log10', 
                                     breaks = c(0.1, 1,10,100,1000,10000),
                                       labels = c(0.1, 1,10,100,1000,10000)) +
  
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  facet_grid(~siteName) +
  scale_color_viridis() + labs(y = "acre-feet per month", x = "water month (1 = Oct, 12 = Sept)")












#}





# ridgelinethickness <- 0.00000001
#ridges <- ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfs, fill = wyaf )) + 
#  geom_ridgeline_gradient(scale = .003, size = ridgelinethickness, alpha = 0.5) + #, min_height = -3500
#  #scale_fill_gradient(colors=turbo())  +
#  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
#  scale_fill_viridis( name = "acre feet     ", direction = 1) +
#  # facet_wrap(~scenario, ncol = 1) + %>%
#  scale_x_continuous(expand = c(0.02,0.02),
#                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
#                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
#                     sec.axis = dup_axis(name = NULL)) +  labs(x = NULL) + #theme_bw() +
#  
#  scale_y_continuous(expand = c(0,0),
#                     breaks = seq(from = 1945, to = 2022, by = 5),
#                     #labels = sjrwytype$wy_wt,
#                     sec.axis = dup_axis(name = NULL)) + labs(y = NULL) + #facet_grid(~name) +#+
#  ggtitle("Amargosa River at Tecopa, USGS Station 10251300\nFull period of record (missing data shown),\nDaily mean flow occurrence and acre-ft per water year") 
##geom_vline(xintercept = 183, linetype = "dashed", color = "red") +
##  geom_vline(xintercept = 305, linetype = "dashed", color = "red")
#ridges
#ggsave("amargosa_tecopa_dailyflow_ridges.jpg", width = 7, height  = 9, dpi = 300, units = "in")
#
#ggplot(annwysum %>% filter(wy!= 2024, wy!=1999), aes(x = wy, y = wyaf, fill = wyaf)) + geom_bar(position = "dodge",stat = "identity") +
#  ggtitle("Amargosa River at Tecopa, USGS Station 10251300\nFull period of record (missing 1973, 1984-1999),\nWater Year accumulated flow volume") +
#  scale_fill_viridis( name = "acre feet"  ) + 
#  geom_hline(yintercept = median_wy_taf, color = "red") + 
#  geom_hline(yintercept = mean_wy_taf, color = "red", linetype = "dashed") + 
#  
#  geom_hline(yintercept = max(annwysum$wyaf), color = "gray", linetype = "dashed") + 
#  geom_hline(yintercept = min_wy_taf, color = "gray", linetype = "dashed") + 
#  
#  
#  
#  scale_y_continuous(breaks = c(0,268, 750,1000,2240, 3000,4000, 5000, 6000, 7000,8000, 9000, 10000, 10792),
#                     labels = c(0,268, 750,1000,2240, 3000,4000, 5000, 6000, 7000,8000, 9000, 10000,10792)) + 
#  scale_x_continuous(breaks=  c(1962, 1970, 1975, 1983, 1990, 1999, 2005, 2010, 2016, 2023),
#                     labels = c(1962, 1970, 1975, 1983, 1990, 1999, 2005, 2010, 2016,  2023)) +
#  labs(x = NULL, y = NULL)
#ggsave("amargosa_tecopa_dailyflow_ridges.jpg", width = 7, height  = 5, dpi = 300, units = "in")
#
#
#annwysum  %>%
#  ggplot(aes(x =  wyaf, y = wy, fill = wyaf, color  = wyaf, label = round(wyaf, 0))) +
#  geom_bar(position = "dodge",stat = "identity") + 
#  theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
#  ylab("taf") + geom_text(color = "dark blue", angle = 90, hjust = 1) + 
#  # scale_x_reordered() +
#  #facet_wrap(~dv, nrow = 1, scales = "free_x") +
#  theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
#  #theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#  scale_y_continuous(sec.axis = dup_axis(name = NULL) )+
#  #scale_fill_manual(values=df_cols) +
#  ggtitle("mean annual (82 yrs)")
#
#
#library(dataRetrieval)
#sites <- whatNWISsites(
#  bBox = c(-117.0, 32.5, -114.0, 36.5),
#  parameterCd = c("00010", "00060"),
#  hasDataTypeCd = "dv" )
#sites
#
#
#
#
#
#
#
#
## hourly
##dischargeUnit <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
##dischargeUnit <- renameNWISColumns(dischargeUnit)#