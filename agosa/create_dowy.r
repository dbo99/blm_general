startdate <- min(df$date, na.rm = TRUE)
enddate <- max(df$date, na.rm = TRUE)

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



write_csv(dfdowy, "daily_dowy.csv")
rm(dfdowy, date, year)