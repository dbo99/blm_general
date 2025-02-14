## SF Eel R NR Miranda CA - 11476500
## Mattole R NR Petrolia CA - 11469000
# https://www.r-bloggers.com/2019/07/bang-bang-how-to-program-with-dplyr/


rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(dataRetrieval)
#library(ggridges)
#library(sf)
#library(viridis)
#library(rgdal)
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

siteNo_sfeel <- "11476500"
pCode <- "00060"
start.date <- "1990-10-01"
end.date <- "2023-09-30"

eel <- readNWISdv(siteNumbers = siteNo_sfeel,
                       parameterCd = pCode,
                       startDate = start.date,
                       endDate = end.date)
eel <- renameNWISColumns(eel) %>% transmute(date = Date, flow = Flow, pnt  = "eel_miranda", usgssite = site_no)


siteNo_mattpetr <- "11469000"
pCode <- "00060"
start.date <- "1990-10-01"
end.date <- "2024-09-30"

matt <- readNWISdv(siteNumbers = siteNo_mattpetr,
                           parameterCd = pCode,
                           startDate = start.date,
                           endDate = end.date)
matt <- renameNWISColumns(matt) %>% transmute(date = Date, flow = Flow, pnt  = "mat_petrolia", usgssite = site_no)

df <- rbind(eel, matt)
#rm(eelatmiranda, mattatpetr)

dowy <- read_csv("daily_dowy.csv")
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date))
df_eel <- df %>% filter(pnt == "eel_miranda")
df_mat <- df %>% filter(pnt == "mat_petrolia")
df_eel_ <- df %>% filter(pnt == "eel_miranda")
df_mat <- df %>% filter(pnt == "mat_petrolia")

rm(dowy)


dlystats_eel <- create_dly_quants(df_eel, dowy, flow)
dlystats_mat <- create_dly_quants(df_mat, dowy, flow)

############
#### plots - Eel - whole water year
############

p <- ggplot() + 
  #geom_line(data = df_hefs, aes(x = dowy, y = kcfs, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2) +
  #geom_line  (data =dlystats_eel,  aes(x=dowy, y=median, linetype = "median"), color = "blue") +
  
  geom_ribbon(data = dlystats_eel,  aes(x=dowy, ymin= `95%`, ymax = max,  fill="5% to max"),  alpha=0.2) +
  geom_ribbon(data = dlystats_eel,  aes(x=dowy, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_eel,  aes(x=dowy, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_eel,  aes(x=dowy, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
  #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=dowy, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
  geom_ribbon(data = dlystats_eel,  aes(x=dowy, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_eel,  aes(x=dowy, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_eel,  aes(x=dowy, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_eel,  aes(x=dowy, ymin= `min`, ymax = `5%`,fill="95% to min"), alpha=0.2) +
  
  geom_line(data = dlystats_eel, aes(x = dowy, y = median), color = "gray2") + 
  geom_line(data = df_eel %>% filter(wy == 2022), aes(x = dowy, y = flow, label = date), color = "red") + 
  geom_line(data = df_eel %>% filter(wy == 2023), aes(x = dowy, y = flow, label = date), color = "orange") + 
  
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL, y = NULL) + #scale_fill_viridis(name = "cfs (15-min)") +
  #geom_line  (data = dtrmnstc,     aes(x=dowy, y=kcfs, color = "deterministic\nforecast")) +
  #geom_point  (data = dtrmnstc,     aes(x=dowy, y=kcfs, color = "deterministic\nforecast")) +
  
  #geom_hline(yintercept = monitorstage_kcfs, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
  #geom_hline(yintercept = floodstage_kcfs, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
  
  #geom_label(data = filter(notify_levs, unit == "kcfs"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
  
  #scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
  #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
  scale_fill_manual(values = c(
    '5% to max' = 'snow4',
    '10 -  5%' = 'yellow',
    '25 - 10%' = 'green', 
    '50 - 25%' = 'blue',
    'median' = 'blue4',
    '75 - 50%' = 'blue',
    '90 - 75%' = 'green',
    '95 - 90%' = 'yellow',
    '95% to min' = 'snow4'),
    breaks = c(
      '5% to max' ,
      '10 -  5%' ,
      '25 - 10%' , 
      '50 - 25%' ,
      'median',
      '75 - 50%' ,
      '90 - 75%' ,
      '95 - 90%',
      '95% to min' )) +
  
  # scale_color_manual(name = "traces", values= c(
  # 'deterministic\nforecast' = 'black')) +
  # labs(y = "cfs", x = NULL) +
  guides(fill=guide_legend(title="mean daily flow (cfs),\nWY1990 thru WY2024")) +
  #guides(color=guide_legend(title="series")) + 
 # scale_linetype_manual(name = "median",values = c(2, 2), 
 #                       guide = guide_legend(override.aes = list(color = c("blue")))) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), limits = c(0,70000)) + 
  ggtitle("Eel at Miranda, daily flow percentiles, red = WY22, orange = WY23, black = median")
p
ggsave("eel.pdf",  dpi = 300, width = 17, height = 11, units = "in")
ggplotly(p)
############
#### plots - Mattole - water year
############


p <- ggplot() + 
  #geom_line(data = df_hefs, aes(x = dowy, y = kcfs, group = mefp_yr, color = 'individual\nensemble\ntrace ("year")') , size = 0.2) +
  #geom_line  (data =dlystats_eel,  aes(x=dowy, y=median, linetype = "median"), color = "blue") +
  
  geom_ribbon(data = dlystats_mat,  aes(x=dowy, ymin= `95%`, ymax = max,  fill="5% to max"),  alpha=0.2) +
  geom_ribbon(data = dlystats_mat,  aes(x=dowy, ymin= `90%`, ymax = `95%`,fill="10 -  5%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_mat,  aes(x=dowy, ymin= `75%`, ymax = `90%`,fill="25 - 10%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_mat,  aes(x=dowy, ymin= `50%`, ymax = `75%`,fill="50 - 25%"),   alpha=0.2) +
  #  geom_ribbon(data = filter(df_qntls_h, qtype =4),  aes(x=dowy, ymin= median, ymax = median,fill="median"),   alpha=0.9) +
  geom_ribbon(data = dlystats_mat,  aes(x=dowy, ymin= `25%`, ymax = `50%`,fill="75 - 50%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_mat,  aes(x=dowy, ymin= `10%`, ymax = `25%`,fill="90 - 75%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_mat,  aes(x=dowy, ymin= `5%`, ymax = `10%`,fill="95 - 90%"),   alpha=0.2) +
  geom_ribbon(data = dlystats_mat,  aes(x=dowy, ymin= `min`, ymax = `5%`,fill="95% to min"), alpha=0.2) +
  
  geom_line(data = dlystats_mat, aes(x = dowy, y = median), color = "gray2") + 
  geom_line(data = df_mat %>% filter(wy == 2022), aes(x = dowy, y = flow, label = date), color = "red") + 
  geom_line(data = df_eel %>% filter(wy == 2023), aes(x = dowy, y = flow, label = date), color = "orange") + 
  
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL, y = NULL) + #scale_fill_viridis(name = "cfs (15-min)") +
  #geom_line  (data = dtrmnstc,     aes(x=dowy, y=kcfs, color = "deterministic\nforecast")) +
  #geom_point  (data = dtrmnstc,     aes(x=dowy, y=kcfs, color = "deterministic\nforecast")) +
  
  #geom_hline(yintercept = monitorstage_kcfs, linetype = "dashed", color = "orange", show.legend = F, size = 2.000) + 
  #geom_hline(yintercept = floodstage_kcfs, linetype = "dashed", color = "red", show.legend = F, size = 2.000) + 
  
  #geom_label(data = filter(notify_levs, unit == "kcfs"), aes(x = midpoint, y = value, label = paste0(type," (",value_t,")")), show.legend = F) + 
  
  #scale_x_datetime(expand = c(0.015, 0.015), name = NULL) +
  #   date_breaks = "1 day", date_minor_breaks = "12 hours", date_labels = "%b %d\n%H:%M pst") +
  scale_fill_manual(values = c(
    '5% to max' = 'snow4',
    '10 -  5%' = 'yellow',
    '25 - 10%' = 'green', 
    '50 - 25%' = 'blue',
    'median' = 'blue4',
    '75 - 50%' = 'blue',
    '90 - 75%' = 'green',
    '95 - 90%' = 'yellow',
    '95% to min' = 'snow4'),
    breaks = c(
      '5% to max' ,
      '10 -  5%' ,
      '25 - 10%' , 
      '50 - 25%' ,
      'median',
      '75 - 50%' ,
      '90 - 75%' ,
      '95 - 90%',
      '95% to min' )) +
  
  # scale_color_manual(name = "traces", values= c(
  # 'deterministic\nforecast' = 'black')) +
  # labs(y = "cfs", x = NULL) +
  guides(fill=guide_legend(title="mean daily flow (cfs),\nWY1990 thru WY2024")) +
  #guides(color=guide_legend(title="series")) + 
  # scale_linetype_manual(name = "median",values = c(2, 2), 
  #                       guide = guide_legend(override.aes = list(color = c("blue")))) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), limits = c(0,50000)) + 
  ggtitle("Mattole at Petrolia, daily flow percentiles, red = WY22, orange = WY23, black = median")
p
ggplotly(p)
ggsave("mattole.pdf",  dpi = 300, width = 17, height = 11, units = "in")




################# 
######  spaghetti
####################

p <-ggplot(df_eel, aes(x = dowy, y = flow, group = wy, label = date, color = as.factor(wy))) + geom_line()
ggplotly(p)

p <-ggplot(df_mat, aes(x = dowy, y = flow, group = wy, label = date, color = as.factor(wy))) + geom_line()
ggplotly(p)
