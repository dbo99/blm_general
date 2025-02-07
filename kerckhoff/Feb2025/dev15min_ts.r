

rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(patchwork)
library(viridisLite)
library(scales)
source("fun_defs.r")



{
## inflows ##
wllw        <- read_csv("final15_wllw.csv")  %>% transmute(date_time = mdy_hm(date_time), cfs = as.double(wllw), trace = "in_wllw", trce = "wllw")
blodm7  <- read_csv("final15_blodm7.csv") %>% transmute(date_time = mdy_hm(date_time), cfs = as.double(blodm7), trace = "in_blodm7", trce = "blodm7")
bc4ph   <- read_csv("final15_bc4ph.csv")     %>% transmute(date_time = mdy_hm(date_time), cfs = as.double(bc4ph), trace = "in_bc4ph", trce = "bc4ph")
agwph <- wllw %>% transmute(date_time, cfs = 200, trace = "in_agwph", trce = "agwph") #no data available..https://www.waterboards.ca.gov/water_issues/programs/tmdl/records/region_5/2006/ref2898.pdf

inflow_15min <- rbind(wllw, blodm7, bc4ph, agwph) %>% mutate(flow = "kerckhoff inflows", unit = "cfs")

## outflows ##

blokhf_j2  <- read_csv("sja_cdec_por1998on.csv")   %>% transmute(date_time = ymd_hms(`OBS DATE`), cfs = as.double(VALUE), trace = "out_blokhf_j2", trce = "blokhf_j2")
k2ph <- wllw %>% transmute(date_time, trace = "k2ph", cfs = 3117, trace = "out_k2ph", trce = "k2ph")

outflow_15min <-rbind(blokhf_j2, k2ph) %>% mutate(flow = "kerckhoff outflows", unit = "cfs")

df_15min <- rbind(inflow_15min, outflow_15min) %>% mutate(type = "flow", date = date(date_time), wy = water_year(date), wm = water_month(date),
                                                          hour = hour(date_time), minute = minute(date_time))


dowy <- read_csv("daily_dowy.csv") %>% mutate(date = ymd(date))
df_15min <- inner_join(df_15min, dowy)

minute <- c(0,15,30,45)
hourfrac <- c(0,0.25, 0.5, 0.75)
minfractab <- data.frame(minute,hourfrac)
head(minfractab)

df_15min <- right_join(df_15min, minfractab)
head(df_15min)

df_15min <- df_15min %>% mutate(hour = as.double(hour) + hourfrac, date = date(date_time))
head(df_15min)

df_15min <- inner_join(df_15min, dowy)
head(df_15min)

df_15min <- df_15min %>% mutate(dayfrac = hour/24)
head(df_15min)
tail(df_15min)

## 15 min

df_15min <- df_15min %>% mutate(dowy_hr = dowy + dayfrac)
head(df_15min)
tail(df_15min)


## replace k2ph static constant with daily means
k2ph_15mintofill <- df_15min %>% filter(trce == "k2ph") %>% select(-cfs)
k2ph_dly <- read_csv("k2p.csv") %>% transmute(date = mdy(date), cfs = af*0.50412 ) 
k2ph <- right_join(k2ph_15mintofill, k2ph_dly)
df_15min <- df_15min %>% filter(trce != "k2ph")
df_15min <- rbind(df_15min, k2ph) %>% mutate(cfs = round(cfs,0))

rm(list=setdiff(ls(), c("df_15min", "water_month", "water_year", "water_week")))
#}


########### total inflow and outflow  #########

df_15min_inoutsum <- df_15min %>% group_by(flow, date_time) %>% summarize(cfs = sum(cfs)) %>% ungroup()
                    
df_15min_inoutsum <- na.omit(df_15min_inoutsum)  %>% mutate(flowtotal = ifelse(flow == "kerckhoff inflows", "inflow total", "outflow total")) %>%
                                                     mutate(trce = ifelse(flow == "kerckhoff inflows", "inflow sum", "outflow sum")) %>%
                                                    transmute(date_time, cfs, flow, trce)


df_15min_lite <- df_15min %>%  transmute(date_time, trce, flow, cfs)
df_15min_lite <- rbind(df_15min_lite, df_15min_inoutsum) %>% mutate(wy = water_year(date_time), date = date(date_time))
df_15min_lite <- na.omit(df_15min_lite)

df_15min_lite$trce <- factor(df_15min_lite$trce , levels =     c("agwph",  "bc4ph",  "wllw", "blodm7", "k2ph", "blokhf_j2", "inflow sum", "outflow sum"))


### storage ############

source("kerstor.r") 
kerstor_dly <- kerstor_dly %>% transmute(date, af = value) 
af_diff <-  c(0 ,diff(kerstor_dly$af))
kerstor_dly_diff <- cbind(kerstor_dly, af_diff)
df_15min_w_stor <- right_join(df_15min, kerstor_dly_diff) %>% mutate(facetlabel = "daily storage (12am)") #refine if time
df_15min_w_stor$trce <- factor(df_15min_w_stor$trce , levels = c("agwph",  "bc4ph",  "wllw", "blodm7", "k2ph", "blokhf_j2", "inflow sum", "outflow sum"))
rm(af_diff)

}



## inflows outflows and storage (geom_line & geom_point())

## early sept to mid Oct

start_dt  <- "2024-09-05 0:00"
end_dt <- "2024-10-13 0:00"
p1 <-  ggplot(df_15min_w_stor %>% filter(date_time >= start_dt, date <= end_dt, 
                                trce != "inflow sum" , trce != "outflow sum"), #line only used to match y-axes with flows
              aes(date_time, cfs, fill = trce)) +
  geom_area() + facet_wrap(~flow, ncol = 1)  +
  scale_fill_manual(values = c("agwph" = "aquamarine1",
                               "bc4ph" = "slateblue3",
                               "wllw"= "pink",
                               "blodm7" = "darkorange1",
                               "k2ph" =     "springgreen4",
                               "blokhf_j2" = "black",
                               "inflow sum" = "purple",
                               "outflow sum" = "yellow")) +
  labs (x= NULL)
p1
#p2 <-  ggplot(df_15min_w_stor %>% filter(date >= "2024-09-04", date <= "2024-10-12", trce == "inflow sum" | trce == "outflow sum")  , 
#             aes(date_time, cfs), color = "black") +
#  geom_area() + facet_wrap(~trce, ncol = 1) +labs(x = NULL)
#p2
p3 <-  ggplot(df_15min_w_stor %>% filter(date_time >= start_dt, date <= end_dt, trce != "inflow sum" | trce != "outflow sum")  ,
              aes(date_time, af))  + geom_line(color = NA) + facet_wrap(~facetlabel) +
  scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%a %d\n%H:%M") +
  geom_point(data = df_15min_w_stor %>% filter(date_time >= start_dt, date <= end_dt,
                                               hour == 0.00)) + 
  geom_line(data = df_15min_w_stor %>% filter(date_time >= start_dt, date <= end_dt,
                                              hour == 0.00)) + 
  geom_hline(yintercept= 150.51, color = "red", linetype = "dashed") +
  geom_hline(yintercept= 1632.61, color = "red") + 
  
  geom_hline(yintercept= 2325.17, color = "red") +
  geom_hline(yintercept= 2818, color = "red", linetype = "dashed") +
  
  
  scale_y_continuous(breaks = c(150.51,500, 1030,  1632.61, 2325, 2818),
                     labels = c("151 (952 ft)","500 (966.7 ft)",  "1000 (975 ft)", "1633 (980 ft) (BOS)", "2325 (984.5 ft) (TOS)", "2818 (987.16 ft) (TRT)"),
                                position = "right")
    
p <- p1/p3
p <- p + plot_layout( heights = c(3,  1)) + labs(x = NULL)
p
ggsave( paste0("3rows_inflowoutflowstorage_", date(start_dt), "_",date(end_dt),".jpg"), dpi = 600, width = 16, height = 9, units = "in", limitsize = FALSE) 
#ggsave( "Jan16.pdf", dpi = 300, width = 16, height = 9, units = "in") ## geom_line with storage


###############################
## late sept to mid Oct
#############################









































































## ridge explore
library(ggridges)
library(viridis)
library(plotly)
ridgelinethickness <- 0.00000001
ggplot(df_15min %>% filter(date >= mdy("9/20/2024"), date <= mdy("10/12/2024")),
                           #trce == "blokhf_j2" | trce == "blodm7"), 
                           aes(dowy_hr, wy, group = wy, height = cfs, fill = cfs )) + 
  geom_ridgeline_gradient(scale = 0.000052, min_height = 10, size = ridgelinethickness) + 
  #scale_fill_gradient(colors=turbo())  +
  #scale_fill_viridis(option="turbo") +
  #facet_grid(~scenario) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL, y = NULL) + scale_fill_viridis(name = "stor_12am (af)") +
  ggtitle("available 15-min data ") + facet_wrap(~trace, ncol = 1) 
  ggsave( "explore.jpg", dpi = 300, width = 16, height = 9, units = "in") 
  
######################
## geom_line
#####################
 
#######################   
#### 15 min ###
########################
p1 <-  ggplot(df_15min_lite %>% filter(wy >= 2023) , aes(date_time, cfs, color = trce, linetype = flow)) +
    geom_line() + facet_grid(~flow)
p1
ggplotly(p1)  

## geom_line - just fall
p1 <-  ggplot(df_15min_lite %>% filter(date >= "2024-09-15", date <= "2024-10-15") , aes(date_time, cfs, color = trce)) + facet_wrap(~flow) +
       geom_line() 
p1
ggplotly(p1)  

p3 <-  ggplot(df_15min_lite%>% filter(date >= "2024-09-15", date <= "2024-10-15") , aes(date_time, cfs, color = trce, linetype = flow)) +
  geom_line() 
p3
ggplotly(p3)



  
  


p1ly <- ggplotly(p1)
p2ly <- ggplotly(p2)
subplot(p1ly, p2ly, nrows=2)



#############################################
#### 15 min to daily ### (not raw daily)
###############################

p1 <-  ggplot(df_15min_todly %>% filter(wy >= 2023) , aes(date, cfs, color = trce, linetype = flow)) +
  geom_line() + facet_wrap(~flow, ncol = 1)
p1
ggplotly(p1)  

p2 <-  ggplot(df_15min_todly %>% filter(date >= "2024-09-01", date <= "2024-10-15") , aes(date, cfs, color = trce, linetype = flow)) +
  geom_line() + geom_line(data = )
p2
ggplotly(p2)  
