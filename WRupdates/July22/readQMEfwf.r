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
dowy <- read_csv("daily_dowy.csv") %>% mutate(date = ymd(date))
head(dowy)



### PFTC1 #####

#pftc1_old <- read_fwf("NDPC1_dailyFNF_1Oct1960_30June2020.txt", skip = 10, fwf_widths(c(21, 15), c("delete", "cfsd"))) %>% transmute(cfsd)
pftc1_old <- read_fwf("PFTC1_dailyFNF_1Oct1955_30June2021.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
  transmute(cfsd)
head(pftc1_old)
tail(pftc1_old)
date <- seq(ymd('1955-10-01'),ymd('2021-06-30'), by = '1 day')
pftc1_old <- data.frame(pftc1_old, date) %>% mutate(nws5id = "PFTC1")
rm(date)
head(pftc1_old)
tail(pftc1_old)
# make sure only complete water years included
pftc1_old <- pftc1_old %>% filter(date <= ymd("2020-09-30"))
tail(pftc1_old)

### HLEC1 #####

#hlec1_old <- read_fwf("NDPC1_dailyFNF_1Oct1960_30June2020.txt", skip = 10, fwf_widths(c(21, 15), c("delete", "cfsd"))) %>% transmute(cfsd)
hlec1_old <- read_fwf("HLEC1_dailyFNF_1Oct1969_30Sep2019.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
  transmute(cfsd)
head(hlec1_old)
tail(hlec1_old)
date <- seq(ymd('1969-10-01'),ymd('2019-09-30'), by = '1 day')
hlec1_old <- data.frame(hlec1_old, date) %>% mutate(nws5id = "HLEC1")
rm(date)
head(hlec1_old)
tail(hlec1_old)
# make sure only complete water years included
hlec1_old <- hlec1_old %>% filter(date <= ymd("2018-09-30"))
tail(hlec1_old)


### EXQC1 #####

#exqc1_old <- read_fwf("NDPC1_dailyFNF_1Oct1960_30June2020.txt", skip = 10, fwf_widths(c(21, 15), c("delete", "cfsd"))) %>% transmute(cfsd)
exqc1_old <- read_fwf("EXQC1_dailyFNF_1Oct1960_31Jul2021.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
  transmute(cfsd)
head(exqc1_old)
tail(exqc1_old)
date <- seq(ymd('1960-10-01'),ymd('2021-07-31'), by = '1 day')
exqc1_old <- data.frame(exqc1_old, date) %>% mutate(nws5id = "EXQC1")
rm(date)
head(exqc1_old)
tail(exqc1_old)
# make sure only complete water years included
exqc1_old <- exqc1_old %>% filter(date <= ymd("2020-09-30"))
tail(exqc1_old)



### LAMC1 #####

#lamc1_old <- read_fwf("NDPC1_dailyFNF_1Oct1960_30June2020.txt", skip = 10, fwf_widths(c(21, 15), c("delete", "cfsd"))) %>% transmute(cfsd)
lamc1_old <- read_fwf("LAMC1_dailyFNF_1Oct1975_30Sep2019.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cmsd"))) %>% 
  mutate(cfsd = cmsd*35.3147) %>% transmute(cfsd)
head(lamc1_old)
tail(lamc1_old)
date <- seq(ymd('1975-10-01'),ymd('2019-09-30'), by = '1 day')
lamc1_old <- data.frame(lamc1_old, date) %>% mutate(nws5id = "LAMC1")
rm(date)
head(lamc1_old)
tail(lamc1_old)
# make sure only complete water years included
lamc1_old <- lamc1_old %>% filter(date <= ymd("2018-09-30"))
tail(lamc1_old)

### TAHC1 #####

#tahc1_old <- read_fwf("NDPC1_dailyFNF_1Oct1960_30June2020.txt", skip = 10, fwf_widths(c(21, 15), c("delete", "cfsd"))) %>% transmute(cfsd)
tahc1_old <- read_fwf("lamc1_dailyFNF_1Oct1979_30June2019.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cmsd"))) %>% 
             mutate(cfsd = cmsd*35.3147) %>% transmute(cfsd)
head(tahc1_old)
tail(tahc1_old)
date <- seq(ymd('1979-10-01'),ymd('2019-06-30'), by = '1 day')
tahc1_old <- data.frame(tahc1_old, date) %>% mutate(nws5id = "TAHC1")
rm(date)
head(tahc1_old)
tail(tahc1_old)
# make sure only complete water years included
tahc1_old <- tahc1_old %>% filter(date <= ymd("2018-09-30"))
tail(tahc1_old)


### FRAC1 #####

#frac1_old <- read_fwf("NDPC1_dailyFNF_1Oct1960_30June2020.txt", skip = 10, fwf_widths(c(21, 15), c("delete", "cfsd"))) %>% transmute(cfsd)
frac1_old <- read_fwf("FRAC1_dailyFNF_1Oct1960_30June2021.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
  transmute(cfsd)
head(frac1_old)
tail(frac1_old)
date <- seq(ymd('1960-10-01'),ymd('2021-06-30'), by = '1 day')
frac1_old <- data.frame(frac1_old, date) %>% mutate(nws5id = "FRAC1")
rm(date)
head(frac1_old)
tail(frac1_old)
# make sure only complete water years included
frac1_old <- frac1_old %>% filter(date <= ymd("2020-09-30"))
tail(frac1_old)



### ISAC1 #####

#isac1_old <- read_fwf("NDPC1_dailyFNF_1Oct1960_30June2020.txt", skip = 10, fwf_widths(c(21, 15), c("delete", "cfsd"))) %>% transmute(cfsd)
isac1_old <- read_fwf("ISAC1_dailyFNF_1Oct1960_30Sep2019.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
  transmute(cfsd)
head(isac1_old)
tail(isac1_old)
date <- seq(ymd('1960-10-01'),ymd('2019-09-30'), by = '1 day')
isac1_old <- data.frame(isac1_old, date) %>% mutate(nws5id = "ISAC1")
rm(date)
head(isac1_old)
tail(isac1_old)
# make sure only complete water years included
isac1_old <- isac1_old %>% filter(date <= ymd("2019-09-30"))
tail(isac1_old)

### NDPC1 #####

#ndpc1_old <- read_fwf("NDPC1_dailyFNF_1Oct1960_30June2020.txt", skip = 10, fwf_widths(c(21, 15), c("delete", "cfsd"))) %>% transmute(cfsd)
ndpc1_old <- read_fwf("NDPC1_dailyFNF_1Oct1960_30June2020.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
             transmute(cfsd)
head(ndpc1_old)
tail(ndpc1_old)
date <- seq(ymd('1960-10-01'),ymd('2020-06-30'), by = '1 day')
ndpc1_old <- data.frame(ndpc1_old, date) %>% mutate(nws5id = "NDPC1")
rm(date)
head(ndpc1_old)
tail(ndpc1_old)
 # make sure only complete water years included
ndpc1_old <- ndpc1_old %>% filter(date <= ymd("2019-09-30"))
tail(ndpc1_old)

## SHDC1 #####

shdc1_old <- read_fwf("SHDC1_dailyFNF_1Oct1950_30Sep2021.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
             transmute(cfsd)
head(shdc1_old)
tail(shdc1_old)
date <- seq(ymd('1950-10-01'),ymd('2021-09-30'), by = '1 day')
shdc1_old <- data.frame(shdc1_old, date) %>% mutate(nws5id = "SHDC1")
rm(date)
head(shdc1_old)
tail(shdc1_old)
# make sure only complete water years included
shdc1_old <- shdc1_old %>% filter(date <= ymd("2021-09-30"))
tail(shdc1_old)

## ORDC1 #####

ordc1_old <- read_fwf("ORDC1_dailyFNF_1Oct1960_30Sep2019.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
  transmute(cfsd)
head(ordc1_old)
tail(ordc1_old)
date <- seq(ymd('1960-10-01'),ymd('2019-09-30'), by = '1 day')
ordc1_old <- data.frame(ordc1_old, date) %>% mutate(nws5id = "ORDC1")
rm(date)
head(ordc1_old)
tail(ordc1_old)
# make sure only complete water years included
ordc1_old <- ordc1_old %>% filter(date <= ymd("2019-09-30"))
tail(ordc1_old)


## FOLC1 #####

folc1_old <- read_fwf("FOLC1_dailyFNF_1Oct1948_30Sep2019.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
  transmute(cfsd)
head(folc1_old)
tail(folc1_old)
date <- seq(ymd('1948-10-01'),ymd('2019-09-30'), by = '1 day')
folc1_old <- data.frame(folc1_old, date) %>% mutate(nws5id = "FOLC1")
rm(date)
head(folc1_old)
tail(folc1_old)
# make sure only complete water years included
folc1_old <- folc1_old %>% filter(date <= ymd("2019-09-30"))
tail(folc1_old)


## CGEC1 #####

cegc1_old <- read_fwf("CEGC1_dailyFNF_1Oct1979_22Sep2021.txt", skip = 10, fwf_widths(c(6,12,4,15), c("name","monyr", "day", "cfsd"))) %>% 
  transmute(cfsd)
head(cegc1_old)
tail(cegc1_old)
date <- seq(ymd('1979-10-01'),ymd('2021-09-22'), by = '1 day')
cegc1_old <- data.frame(cegc1_old, date) %>% mutate(nws5id = "CEGC1")
rm(date)
head(cegc1_old)
tail(cegc1_old)
# make sure only complete water years included
cegc1_old <- cegc1_old %>% filter(date <= ymd("2020-09-30"))
tail(cegc1_old)


###########################################
############## Plots #####################
###########################################

### Oroville  ####

### ridge example check
df <- ordc1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.000078, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
 # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
 # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1960, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top

## Shasta #####

{
### ridge example check
df <- shdc1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.00006, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1950, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top
}
#################################
## Folsom #####
###############################

### ridge example check
df <- folc1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.00008, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1948, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top

#####################
############ Don Pedro
##########################



### ridge example check
df <- ndpc1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.00015, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1960, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top

#####################
############ Isabella
##########################



### ridge example check
df <- isac1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.00025, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1960, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top


#####################
############ Millerton #############

### ridge example check
df <- frac1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.00017, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1960, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top

############ Tahoe #############

### ridge example check
df <- tahc1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.00017, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1960, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top

############ Lake Mendo #############

### ridge example check
df <- lamc1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.0003, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1960, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top

############ Englebright #############

### ridge example check
df <- hlec1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.00015, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1970, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top


############ Exchequer #############

### ridge example check
df <- exqc1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.00017, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1960, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top


############ Pine Flat #############

### ridge example check
df <- pftc1_old
df <- inner_join(df, dowy) %>% mutate(wy = water_year(date), cfsd = as.double(cfsd), maf = cfsd*1.983459/1000000 )
as_tibble(df)

df_wysum <- df %>% group_by(wy) %>% summarize(wysum = sum(maf,na.rm=TRUE))
head(df_wysum)
tail(df_wysum)

df <- inner_join(df, df_wysum)
head(df)

ridgelinethickness <- 0.00000001
ggplot(df, aes(dowy, wy, group = as.factor(wy), height = cfsd, fill = wysum )) + 
  geom_ridgeline_gradient(scale = 0.00017, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1954, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top

