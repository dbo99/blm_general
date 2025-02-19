

{rm(list = ls())
  
  rstudioapi::getActiveDocumentContext
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  library(tidyverse)
  library(patchwork)
  library(viridisLite)
  library(scales)
  source("fun_defs.r")
  
}


#### synthetic traces ######
source("blmcond4.r")

#### observations #########

{
###### day of water year table  #####
dowy <- read_csv("daily_dowy.csv") %>% mutate(date = ymd(date))
#head(dowy)

###### blo dam 7  #####
blodm7 <- read_csv("blodm7_15min.csv") %>% transmute(date_time = mdy_hm(date_time), 
                                                     date = date(date_time), cfs,
                                                     af = cfs*(0.08264462912563/4),
                                                     trace = "blodm7")
head(blodm7)
bc4ph <- read_csv("bc4ph.csv")  %>% transmute(date_time = mdy_hm(date_time), 
                                              date = date(date_time), cfs,
                                              af = cfs*(0.08264462912563/4),
                                              trace = "bc4ph")
head(bc4ph)


####### millerton full natural flow 
frac1_23 <- read_csv("frac1_wy23.csv") %>% transmute(date = mdy(date), af = kaf*1000)
frac1_24 <- read_csv("frac1_wy24.csv") %>% transmute(date = mdy(date), af = kaf*1000)

frac1_fnf <- rbind(frac1_23, frac1_24) %>% mutate(cfs = af*0.504166)
rm(frac1_23, frac1_24)

frac1_fnf15min <- blodm7 %>% transmute(date_time, date) 
frac1_fnf <- inner_join(frac1_fnf, frac1_fnf15min) %>% mutate(trace = "fnf_frac1")
rm(frac1_fnf15min)
}


########### prepare kerkhoff proposed flows, wet first (schedule d = wet)

khf_ltor_wet <- blmconds_15min %>% filter(cond == "water year type d\ngeneral spill rampdown")
date_time <- seq(ymd_hm('2023-07-26 12:00'),ymd_hm('2023-08-16 11:45'), by = '15 mins') ##manullay set matching period (2016 rows)
khf_ltor_wet <- cbind(khf_ltor_wet, date_time) %>% transmute(date_time, date = date(date_time), cfs, trace =  cond, af = cfs*(0.08264462912563/4))
klw_plus_k2cap <- khf_ltor_wet %>% mutate(cfs = cfs + 4800, trace = "k2cap_ltorwet")
                                                        

############# rbind  ############


df <- rbind(frac1_fnf, blodm7, bc4ph, khf_ltor_wet, klw_plus_k2cap) %>% mutate(wy = water_year(date), wm = water_month(date))



################### water year plot, wet  #################
p1 <- df %>% ggplot() + geom_area(data = df %>% filter(wy == 2023, trace == "blodm7" | trace == "bc4ph"), aes(x = date_time, y = cfs, fill = trace)) +
       geom_line(data = df %>% filter(wy == 2023, trace == "fnf_frac1"), aes(x = date_time, y = cfs, color = trace)) +
 # geom_line(data = df %>% filter(wy == 2023, trace == "water year type d\ngeneral spill rampdown"), aes(x = date_time, y = cfs, color = trace)) +
#  geom_line(data = df %>% filter(wy == 2023, trace == "k2cap_ltorwet"), aes(x = date_time, y = cfs, color = trace)) +
       scale_color_manual(values = c("agwph" = "aquamarine1",
                                  "bc4ph" = "slateblue3",
                                  "wllw"= "pink",
                                  "blodm7" = "darkorange1",
                                  "k2ph" =     "springgreen4",
                                  "blokhf.j2" = "black",
                                  "fnf_frac1" = "black",
                                  "k2cap_ltorwet" = "springgreen4",
                          "water year type d\ngeneral spill rampdown" = "blue")) +
                           
  
  scale_fill_manual(values = c("agwph" = "aquamarine1",
                                "bc4ph" = "slateblue3",
                                "wllw"= "pink",
                                "blodm7" = "darkorange1",
                                "k2ph" =     "springgreen4",
                                "blokhf.j2" = "black",
                                "fnf_frac1" = "black",
                               "k2cap_ltorwet" = "springgreen4",
                               "water year type d\ngeneral spill rampdown" = "blue")) + ggtitle("WY23, all: Full Natural Flow (FNF) above Millerton with two primary Kerckhoff inflows")
                      
#p1
#ggsave("WY23_FRAC1fnf_withBC4&BLODM7.jpeg", width = 16, height = 9, unit = "in")

################### spring melt plot, wet  #################
p2 <- df %>% ggplot() + geom_area(data = df %>% filter(wy == 2023, wm > 9, trace == "blodm7" | trace == "bc4ph"), aes(x = date_time, y = cfs, fill = trace)) +
  geom_line(data = df %>% filter(wy == 2023,wm > 9, trace == "fnf_frac1"), aes(x = date_time, y = cfs, color = trace)) +
  geom_line(data = df %>% filter(wy == 2023,wm > 9, trace == "water year type d\ngeneral spill rampdown"), aes(x = date_time, y = cfs, color = trace)) +
  geom_line(data = df %>% filter(wy == 2023,wm > 9, trace == "k2cap_ltorwet"), aes(x = date_time, y = cfs, color = trace), alpha = 0.5) +
  scale_color_manual(values = c("agwph" = "aquamarine1",
                                "bc4ph" = "slateblue3",
                                "wllw"= "pink",
                                "blodm7" = "darkorange1",
                                "k2ph" =     "springgreen4",
                                "blokhf.j2" = "black",
                                "fnf_frac1" = "black",
                                "k2cap_ltorwet" = "springgreen4",
                                "water year type d\ngeneral spill rampdown" = "blue")) +
  
  
  scale_fill_manual(values = c("agwph" = "aquamarine1",
                               "bc4ph" = "slateblue3",
                               "wllw"= "pink",
                               "blodm7" = "darkorange1",
                               "k2ph" =     "springgreen4",
                               "blokhf.j2" = "black",
                               "fnf_frac1" = "black",
                               "k2cap_ltorwet" = "springgreen4",
                               "water year type d\ngeneral spill rampdown" = "blue")) +

     geom_hline(yintercept = 4800, linetype = "dashed", color = "springgreen4" ) +
    geom_hline(yintercept = 3636, linetype = "dashed", color = "slateblue3" ) + ggtitle("WY23, end of melt: FNF above Millerton with Proposed Schedule D (wet) recession")
p2
ggsave("WY23_endofmelt.jpeg", width = 16, height = 9, unit = "in")

