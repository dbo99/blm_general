#rm(list = ls())

#rstudioapi::getActiveDocumentContext
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#library(tidyverse)
#source("fun_defs.r")

## kerckhoff daily storagd at midnight
## from usgs nwis. dataRetrieval did not work, manually (chatGPT, googlesheets) generated
{
kf2006 <- read_csv("kstor2006.csv") %>% transmute(date = mdy(date), value = mdnghtstor)
kf2007 <- read_csv("kstor2007.csv") %>% transmute(date = ymd(date), value = mdnghtstor)
kf2008 <- read_csv("kstor2008.csv") %>% transmute(date = ymd(date), value = mdnghtstor)
kf2009 <- read_csv("kstor2009.csv") %>% transmute(date = ymd(date), value = mdnghtstor)
kf2010 <- read_csv("kstor2010.csv") %>% transmute(date = ymd(date), value = mdnghtstor)
kf2011 <- read_csv("kstor2011.csv") %>% transmute(date = ymd(date), value = mdnghtstor)
kf2012 <- read_csv("kstor2012.csv") %>% transmute(date = ymd(date), value = mdnghtstor)
kf2013 <- read_csv("kstor2013.csv") %>% transmute(date = ymd(date), value = mdnghtstor)
kf2014 <- read_csv("kstor2014.csv") %>% transmute(date = mdy(date), value = mdnghtstor)
kf2015 <- read_csv("kstor2015.csv") %>% transmute(date = mdy(date), value = mdnghtstor)
kf2016 <- read_csv("kstor2016.csv") %>% transmute(date = ymd(date), value = mdnghtstor)
kf2017 <- read_csv("kstor2017.csv") %>% transmute(date = mdy(date), value = mdnghtstor)
kf2018 <- read_csv("kstor2018.csv") %>% transmute(date = mdy(date), value = mdnghtstor)
kf2019 <- read_csv("kstor2019.csv") %>% transmute(date = mdy(date), value = mdnghtstor)
kf2020 <- read_csv("kstor2020.csv") %>% transmute(date = mdy(date), value = mdnghtstor) }

## define list of usgs sources
kerstor_usgs <- rbind(kf2006, kf2007,kf2008,kf2009,kf2010,kf2011,kf2012,kf2013,kf2014,kf2015,
                      kf2016,kf2017,kf2018,kf2019,kf2020 ) %>% mutate(srce = "nwis")
rm(kf2006, kf2007,kf2008,kf2009,kf2010,kf2011,kf2012,kf2013,kf2014,kf2015,
    kf2016,kf2017,kf2018,kf2019,kf2020 ) 


## cdec has recent kerckhoff midnight storage, first date 2/9/2020

kerstor_cdec <- read_csv("kerstor.csv") %>% transmute(date = mdy(date), value = af, srce = "cdec")
 
## find oldest cdec date

kerstor_cdec_oldest <- min(kerstor_cdec$date)
kerstor_usgs_newest <- max(kerstor_usgs$date)

## only keep non usgs data that doesnt overlap w/cdec
kerstor_usgs_clip <- kerstor_usgs %>% filter(date < kerstor_cdec_oldest)

kerstor_dly <- rbind(kerstor_cdec, kerstor_usgs_clip) %>% mutate(unit = "af", type = "res stor", var = "ker stor")

rm(kerstor_cdec_oldest, kerstor_usgs_newest, kerstor_cdec, kerstor_usgs, kerstor_usgs_clip)


### k2 powerhouse daily af (not cfs)

#k2af_dly <- read_csv("k2p.csv") %>% transmute(date = mdy(date), value = af, srce = "cdec", 
#                                    unit = "af", type = "flow", var = "k2ph")
#
### combine as needed
#
#df_af <- rbind(kerstor_dly, k2af_dly)
#
#
#dowy <- read_csv("daily_dowy.csv") %>% mutate(date = ymd(date))
#
#df_af <- inner_join(df_af, dowy) %>% mutate(wy = water_year(date), wm = water_month(date), wy_wm = paste0(wy, "_", wm)) %>% filter_at(vars(value), all_vars(!is.na(.)))
#
#mon <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
#wm <- c(1,2,3,4,5,6,7,8,9,10,11,12)
#mon <-data.frame(mon, wm)
#
#df_af <- right_join(df_af, mon) %>% mutate(wy_mon = paste0(wy, "_", mon))
#
#df_af$mon <- factor(df_af$mon, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
#
#df_cfs <- df_af %>% mutate(value = value*0.504166, unit = "cfs")
#df_cfs$mon <- factor(df_cfs$mon, levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"))
#
#df <- rbind(df_af, df_cfs)

#eodstor_wyrange <-  df_af %>% filter(type == "res stor") %>% 
#                    group_by(wy) %>% summarize(min = min(value),
#                    max = max(value)) %>% mutate(range = max-min)
#write_csv(eodstor_wyrange, "eodstorage.csv")
