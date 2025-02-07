


sjr_sjaj2 <- read_csv("sja_cdec_por1998on.csv") %>% transmute(date_time = ymd_hms(`OBS DATE`),
             var = "sjr_j2", type = "flow", value = as.double(VALUE), unit = "cfs", 
             wy = water_year(date_time), wm = water_month(date_time),
             hour = hour(date_time), minute = minute(date_time))


## build day of water year table to join to timeseries data
dowy <- read_csv("daily_dowy.csv")
minute <- c(0,15,30,45)
hourfrac <- c(0,0.25, 0.5, 0.75)
minfractab <- data.frame(minute,hourfrac)
head(minfractab)

sjr_sjaj2 <- right_join(sjr_sjaj2, minfractab)
head(sjr_sjaj2)

sjr_sjaj2 <- sjr_sjaj2 %>% mutate(hour = as.double(hour) + hourfrac, date = date(date_time))
head(sjr_sjaj2)

sjr_sjaj2 <- inner_join(sjr_sjaj2, dowy)
head(sjr_sjaj2)

sjr_sjaj2 <- sjr_sjaj2 %>% mutate(dayfrac = hour/24)
head(sjr_sjaj2)
tail(sjr_sjaj2)

## 15 min

sjr_sjaj2 <- sjr_sjaj2 %>% mutate(dowy_hr = dowy + dayfrac)
head(sjr_sjaj2)
tail(sjr_sjaj2)

## daily

sjr_sjaj2_dly <- sjr_sjaj2 %>% group_by(date) %>% summarize(value = mean(value)) %>%
          mutate(wy = water_year(date))

############ join #####

sjr_sjaj2_dly <- inner_join(sjr_sjaj2_dly, dowy) %>% 
                mutate(type = "flow", var = "sjr_j2", unit = "cfs", srce = "cdec")


