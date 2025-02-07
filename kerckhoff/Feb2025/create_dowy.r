startdate <- mdy("10/1/1900")
enddate <- mdy("09/30/2050")

date <- seq.Date(startdate, enddate, by = "day")
year <-  year(date)
dfdowy <- data.frame(date, year) 
head(dfdowy)
df_cumdoy <- read_csv("leap_yrs.csv")  
head(df_cumdoy)

# df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 
dfdowy <- inner_join(dfdowy, df_cumdoy)
head(df)

dfdowy <- dfdowy %>% mutate(yday = yday(date))
head(df)



dfdowy <- dfdowy %>% mutate(dowy =   ifelse(cumdoy > 365, 
                                            ifelse(yday>274, yday-274, yday+92), 
                                            ifelse(yday>273, yday-273, yday+92)))



dfdowy <- dfdowy %>% mutate(week = week(date), wtrwk = ifelse(week>39, week-39, week+13),
                                               wtrwk = ifelse(dowy == 366, 52, wtrwk))
                                                          

write_csv(dfdowy, "daily_dowy.csv")
rm(df_cumdoy, dfdowy, date, year)
