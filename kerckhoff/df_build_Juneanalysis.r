rm(list = ls())
library(tidyverse)
library(lubridate)
library(ggridges)
library(viridis)
library(zoo)
library(plotly)
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("fun_defs.r")
daily_dowy <- read_csv("daily_dowy.csv")

# without scenario (baseline)

k1_co <- read_csv("k1_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "k1")
k2_co <- read_csv("k2_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "k2")
sjr_blokrhff_co <- read_csv("sjrblo_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "sjr_blokrhff")
outge_co <- read_csv("outge_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "outage")
splprtcl_co <- read_csv("splprtcl_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "spill_prtcl")
whtwtr_co <- read_csv("whtwtr_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "whtwtr_rel")
resinfl_co <- read_csv("inflow_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "inflow")
  
reselev_co <- read_csv("elev_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = ft, unit = "ft", dv = "res_elev")
resstor_co <- read_csv("stor_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = af, unit = "af", dv = "res_stor")

temp_co <- read_csv("temp_wo.csv") %>% transmute(date_time = dmy_hm(date_time), value = degf, unit = "degf", dv = "temp")

df_co <- rbind(k1_co, k2_co, sjr_blokrhff_co, outge_co, splprtcl_co, whtwtr_co,
               resinfl_co, reselev_co, resstor_co, temp_co) %>% mutate(scenario = "baseline")
#rm(k1, k2, sjr_blokrhff, outge, splprtcl, whtwtr)

# with scenario (proposed action)

k1_pa <- read_csv("k1_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "k1")
k2_pa <- read_csv("k2_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "k2")
sjr_blokrhff_pa <- read_csv("sjrblo_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "sjr_blokrhff")
outge_pa <- read_csv("outge_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "outage")
splprtcl_pa <- read_csv("splprtcl_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "spill_prtcl")
whtwtr_pa <- read_csv("whtwtr_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "whtwtr_rel")
resinfl_pa <- read_csv("inflow_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = cfs, unit = "cfs", dv = "inflow")

reselev_pa <- read_csv("elev_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = ft, unit = "ft", dv = "res_elev")
resstor_pa <- read_csv("stor_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = af, unit = "af", dv = "res_stor")

temp_pa <- read_csv("temp_wi.csv") %>% transmute(date_time = dmy_hm(date_time), value = degf, unit = "degf", dv = "temp")

df_pa <- rbind(k1_pa, k2_pa, sjr_blokrhff_pa, outge_pa, splprtcl_pa, whtwtr_pa,
               resinfl_pa, reselev_pa, resstor_pa, temp_pa) %>% mutate(scenario = "proposed action")
#rm(k1, k2, sjr_blokrhff, outge, splprtcl, whtwtr)

df <- rbind(df_co, df_pa)

#rm (k1_co, k2_co, sjr_blokrhff_co, outge_co, splprtcl_co, whtwtr_co,
#    k1_pa, k2_pa, sjr_blokrhff_pa, outge_pa, splprtcl_pa, whtwtr_pa)#,
    #df_co, df_pa)

df <- df %>% mutate(wy = water_year(date_time), wm = water_month(date_time),
                    hour = hour(date_time), minute = minute(date_time))
head(df)

minute <- c(0,15,30,45)
hourfrac <- c(0,0.25, 0.5, 0.75)
minfractab <- data.frame(minute,hourfrac)
head(minfractab)

df <- right_join(df, minfractab)
head(df)

df <- df %>% mutate(hour = as.double(hour) + hourfrac, date = date(date_time))
head(df)

df <- df %>% transmute(date_time, value, unit, dv, value, scenario, wy, wm, hour, date)
head(df)

df <- inner_join(df, daily_dowy)
head(df)

df <- df %>% mutate(dayfrac = hour/24)
head(df)
tail(df)

df <- df %>% mutate(dowy_hr = dowy + dayfrac)
head(df)
tail(df)

df <- df %>% transmute(date_time, value, unit, dv, scenario, wy, wm, date, year, dowy_hr)

#rm(daily_dowy, df_co, df_pa, minfractab)


################# Build DF_Diff #############
#############################################
## beware k1_co overwritten, so whole script must be used
##################################

k1_co <- k1_co %>% transmute(value_bl = value)
k1 <- cbind(k1_co, k1_pa) %>% transmute(date_time, value = value-value_bl, unit, dv )

k2_co <- k2_co %>% transmute(value_bl = value)
k2 <- cbind(k2_co, k2_pa) %>% transmute(date_time, value = value-value_bl, unit, dv )

sjr_blokrhff_co <- sjr_blokrhff_co %>% transmute(value_bl = value)
sjr_blokrhff <- cbind(sjr_blokrhff_co, sjr_blokrhff_pa) %>% transmute(date_time, value = value-value_bl, unit, dv )

outge_co <- outge_co %>% transmute(value_bl = value)
outge <- cbind(outge_co, outge_pa) %>% transmute(date_time, value = value-value_bl, unit, dv )

splprtcl_co <- splprtcl_co %>% transmute(value_bl = value)
splprtcl <- cbind(splprtcl_co, splprtcl_pa) %>% transmute(date_time, value = value-value_bl, unit, dv )  
  
whtwtr_co <- whtwtr_co %>% transmute(value_bl = value)
whtwtr <- cbind(whtwtr_co, whtwtr_pa) %>% transmute(date_time, value = value-value_bl, unit, dv )  

resinfl_co <- resinfl_co %>% transmute(value_bl = value)
resinfl <- cbind(resinfl_co, resinfl_pa) %>% transmute(date_time, value = value-value_bl, unit, dv )  

reselev_co <- reselev_co %>% transmute(value_bl = value)
reselev <- cbind(reselev_co, reselev_pa) %>% transmute(date_time, value = value-value_bl, unit, dv )  

resstor_co <- resstor_co %>% transmute(value_bl = value)
resstor <- cbind(resstor_co, resstor_pa) %>% transmute(date_time, value = value-value_bl, unit, dv ) 

temp_co <- temp_co %>% transmute(value_bl = value)
temp <- cbind(temp_co, temp_pa) %>% transmute(date_time, value = value-value_bl, unit, dv )  


df_diff <- rbind(k1, k2, sjr_blokrhff, outge, splprtcl, whtwtr, resinfl, reselev, resstor, temp) %>% 
            #mutate(scenario = "baseline minus proposed action")
            mutate(scenario = "proposed action minus baseline")
head(df_diff)

df_diff <- df_diff %>% mutate(wy = water_year(date_time), wm = water_month(date_time),
                    hour = hour(date_time), minute = minute(date_time))
head(df_diff)

minute <- c(0,15,30,45)
hourfrac <- c(0,0.25, 0.5, 0.75)
minfractab <- data.frame(minute,hourfrac)
head(minfractab)

df_diff <- right_join(df_diff, minfractab)
head(df_diff)



df_diff <- df_diff %>% mutate(hour = as.double(hour) + hourfrac, date = date(date_time))
head(df_diff)

df_diff <- df_diff %>% transmute(date_time, value, unit, dv, value, scenario, wy, wm, hour, date)
head(df_diff)

df_diff <- inner_join(df_diff, daily_dowy)
head(df_diff)

df_diff <- df_diff %>% mutate(dayfrac = hour/24)
head(df_diff)
tail(df_diff)

df_diff <- df_diff %>% mutate(dowy_hr = dowy + dayfrac)
head(df_diff)
tail(df_diff)

df_diff <- df_diff %>% transmute(date_time, value, unit, dv, scenario, wy, wm, date, year, dowy_hr)

rm(list=ls()[! ls() %in% c("df","df_diff")])
head(df_diff)
tail(df_diff)

sjrwytype <- read_csv("sjr_wytype.csv") %>% filter(wy >= 2003, wy <= 2017) 
                                          



sjrwytype <- sjrwytype %>% mutate(sjwyt = trimws(sjwyt),sjwytf = trimws(sjwytf),
                                  wy_sjwytf = paste0("wy", wy, "_", sjwytf))
df <- right_join(df, sjrwytype)
df_diff <- right_join(df_diff, sjrwytype)
head(df_diff)
tail(df_diff)

wm <- c(1,2,3,4,5,6,7,8,9,10,11,12)
wmt <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
wm <- data.frame(wm, wmt)

df <- right_join(df, wm)
df_diff <- right_join(df_diff, wm)



df$wmt <- factor(df$wmt, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
df_diff$wmt <- factor(df_diff$wmt, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))


df$sjwytf <- factor(df$sjwytf, levels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical"))
df_diff$sjwytf <- factor(df_diff$sjwytf, levels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical"))
head(df)
head(df_diff)



df <- df %>% filter(wy != 2004)
df_diff <- df_diff %>% filter(wy != 2004)

df <- df %>% mutate(mon_yr_wy = paste0(mon_yr_wy = as.yearmon(date_time), " - ", sjwytf , " sjwyt"))
df_diff <- df_diff %>% mutate(mon_yr_wy = paste0(mon_yr_wy = as.yearmon(date_time), " - ", sjwytf , " sjwyt"))

df <- df %>% mutate(wmt_sjwytf = paste0(wmt, " - ", sjwytf , " sjwyt"))
df_diff <- df_diff %>% mutate(wmt_sjwytf = paste0(wmt, " - ", sjwytf , " sjwyt"))

df <- df %>% mutate(dv_sjwytf = paste0(dv, " - ", sjwytf , " sjwyt"))
df_diff <- df_diff %>% mutate(dv_sjwytf = paste0(dv, " - ", sjwytf , " sjwyt"))

df <- df %>% mutate(dv_wy_sjwytf = paste0(dv, " - ", wy_sjwytf , " sjwyt"))
df_diff <- df_diff %>% mutate(dv_wy_sjwytf = paste0(dv, " - ", wy_sjwytf , " sjwyt"))

dvs <- c("k1" , "k2","sjr_blokrhff", "outage","spill_prtcl",
         "whtwtr_rel","inflow","res_elev","res_stor","temp")
#write_csv(df, "df.csv")
#write_csv(df_diff, "df_diff.csv")

#monyrmeandiff <- df_diff %>% group_by(dv, mon_yr_wy) %>% summarize(monyrmeandiff = mean(value))
#meanmonwyt <- df_diff %>% group_by(dv, wmt_sjwytf) %>% summarize(meanmonwyt = mean(value))
#monmeandiff <- df_diff %>% group_by(dv, wmt) %>% summarize(monmeandiff = mean(value))
#

mean__mon_yr_wy <- df_diff %>% group_by(dv, mon_yr_wy) %>% summarize(mean_mon_yr_wy_diff = mean(value, na.rm = TRUE)) #for 168
mean__wmt_sjwytf <- df_diff %>% group_by(dv, wmt_sjwytf) %>% summarize(mean_wmt_sjwytf_diff = mean(value, na.rm = TRUE)) # for 60
mean__wmt <- df_diff %>% group_by(dv, wmt) %>% summarize(mean_wmt_diff = mean(value, na.rm = TRUE))

df_diff <- right_join(df_diff, mean__mon_yr_wy)
df_diff <- right_join(df_diff, mean__wmt_sjwytf)
df_diff <- right_join(df_diff, mean__wmt)

sjrwytype <- sjrwytype %>% mutate(wy_wt = paste0(wy, " ", sjwyt))

df <- df %>% mutate(scen_dv = paste0(scenario,"_", dv))
df_diff <- df_diff %>% mutate(scen_dv = paste0(scenario,"_", dv))


df$dv <- factor(df$dv, levels = c("inflow", "k1" ,"k2" , "res_stor", "res_elev", "outage", "spill_prtcl" , "whtwtr_rel" ,"temp", "sjr_blokrhff" ))



df$scenario <- factor(df$scenario, levels = c("proposed action", "baseline"))
unique(df$scenario)

dvs <- c("k1" , "k2","sjr_blokrhff", "outage","spill_prtcl","whtwtr_rel","inflow","res_elev","res_stor","temp")

df <- df %>% mutate(dv_month_scen = paste0(dv, "_", wmt, "_", scenario))
df <- df %>% mutate(dv_monthyr_scen = paste0(dv, "_", wmt_sjwytf, "_", scenario))
df <- df %>% mutate(dv_eachmonthyr_scen = paste0(dv, "_", mon_yr_wy, "_", scenario))

df_diff <- df_diff %>% mutate(dv_month_scen = paste0(dv, "_", wmt, "_", scenario))
df_diff <- df_diff %>% mutate(dv_monthyr_scen = paste0(dv, "_", wmt_sjwytf, "_", scenario))
df_diff <- df_diff %>% mutate(dv_eachmonthyr_scen = paste0(dv, "_", mon_yr_wy, "_", scenario))
head(df_diff)


#########################################
################# June reanalyis
########################################

sjrbloaf <- df %>% filter(dv == "sjr_blokrhff") %>% transmute(date_time, value, unit, dv, scenario, wy, wm, date, year, dowy_hr, sjwyt) %>%
            mutate(af = value*0.00137741046832*15) #15 minute data conversion from cfs to af
head(sjrbloaf)

# what dates have the biggest difference in volume of releases?

sjrbloaf_dailyvol <- sjrbloaf %>% group_by(scenario, date) %>% summarize(af_day = sum(af)) 
head(sjrbloaf_dailyvol)
sjrbloaf_dailyvol <- sjrbloaf_dailyvol %>% pivot_wider(names_from = scenario, values_from = af_day) %>% mutate(scenariodiff = `proposed action` - baseline)
head(sjrbloaf_dailyvol)
sjrbloaf_dailyvol <- sjrbloaf_dailyvol %>% arrange(desc(scenariodiff)) %>% mutate(wy = water_year(date)) %>% inner_join(sjrwytype)
head(sjrbloaf_dailyvol)
sjrbloaf_dailyvol <- sjrbloaf_dailyvol %>% inner_join(daily_dowy) %>% inner_join(labelcols)

labelcols <- c( "orange", "red", "dark green", "dark green", "coral4", "coral4", "orange",
                "light green", "dark green", "red", "coral4", "coral4", "coral4", "coral4", "dark green")

ggplot(sjrbloaf_dailyvol, aes(x = wy, y = dowy, fill = scenariodiff)) + geom_tile() +
  #scale_fill_viridis( option = "turbo", name = "5635") 
  scale_fill_gradient2( name = "Daily\nVolume\nScenario\nDifference (af)") + 
  scale_y_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365), 
                     #breaks = c(365, 336,305,274,244,213,183,152,124,93,62, 32,1),
                     labels = c("Oct Begin", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug", "Sep", "Oct end"),
                     sec.axis = dup_axis(name = NULL)) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(from = 2003, to = 2017, by = 1),
                     labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, color = labelcols)) +
  theme(legend.key.height = unit(1, "in"))+   theme(legend.key.width = unit(1, "in"))+ labs(y = NULL)+
   ggtitle("San Joaquin River below Kerckhoff Dam, release volume difference [Daily]\nCurrent Ops minus Proposed FERC Re-licensing Action (15-min output, 10 Mar '22 output)\ncapital = more obvious, lowercase = less obvious (see enclosed)")

ggsave("DailyVolumeKerckhoffReleaseDifference.jpg", dpi = 300, width = 17, height = 11, units = "in") 

###############################
#### Diffevents zoom in 
###############################

diffevents <- read_csv("diffevents.csv") %>% mutate(start_date = mdy(start_date), end_date = mdy(end_date),
                                             wy = water_year(start_date)) %>% inner_join(sjrwytype)
head(diffevents)

for(i in 1:length(diffevents$letter)) { 
  start <- diffevents$start_date[i]
  end <- diffevents$end_date[i]

 p <-  ts_plot(df %>% filter(dv == "sjr_blokrhff"),start,end) +
    ggtitle(paste0("Model Output Divergence Event: ", diffevents$letter[i], "        ", start, " to ", end, 
                   ", SJR below Kerckhoff, 15-min March '22 ResSim output")) + labs(y = "CFS (15-min avg)", sec.axis = dup_axis) +
    theme(text=element_text(size=21)) + scale_y_continuous(sec.axis = dup_axis (name = NULL))
 p
  ggsave(paste0(diffevents$letter[i]," - ", diffevents$wy_sjwytf[i],"_", start, " to ", end, 
                ".jpg"), dpi = 300, width = 22, height = 13, units = "in")
  z <- ggplotly(p)
  htmlwidgets::saveWidget(as_widget(z), paste0(diffevents$letter[i]," - ", diffevents$wy_sjwytf[i],"_", start, " to ", end, 
                                               ".html"))
  
  }


ggplot(df %>% filter(dv == "sjr_blokrhff", date >= start, date <= end), aes(x = date_time, y = value, color = scenario, linetype=scenario)) + geom_line()
ggsave("restest.jpg", dpi = 300, width = 17, height = 11, units = "in") 







# what months have the biggest difference in volume of releases?

#sjrbloaf_monthlyvol <- sjrbloaf %>% group_by(scenario, wm, wy) %>% summarize(af_mon = sum(af)) 
#head(sjrbloaf_monthlyvol)
#sjrbloaf_monthlyvol <- sjrbloaf_monthlyvol %>% pivot_wider(names_from = scenario, values_from = af_mon) %>% mutate(scenariodiff = `proposed action` - baseline)
#head(sjrbloaf_monthlyvol)
#sjrbloaf_monthlyvol <- sjrbloaf_monthlyvol %>% arrange(desc(scenariodiff))  %>% inner_join(sjrwytype)
#head(sjrbloaf_monthlyvol)
#
#ggplot(sjrbloaf_monthlyvol, aes(x = wy, y = wm, fill = scenariodiff)) + geom_tile() +
#  scale_fill_viridis( option = "turbo", name = "5635") 



