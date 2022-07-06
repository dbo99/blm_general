start_date <- ymd("1930-10-01")
end_date <- ymd("2028-09-30")

date <- seq.Date(start_date, end_date, by = "day")
year <-  year(date)
df <- data.frame(date, year) 
head(df)
df_cumdoy <- read_csv("leap_yrs.csv")  
head(df_cumdoy)

# df$cumdoy <- df_cumdoy$cumdoy[match(df$year,df_cumdoy$year)] 
df <- inner_join(df, df_cumdoy)
head(df)

df <- df %>% mutate(yday = yday(date))
head(df)



df <- df %>% mutate(dowy =   ifelse(cumdoy > 365, 
                                    ifelse(yday>274, yday-274, yday+92), 
                                    ifelse(yday>273, yday-273, yday+92)))
head(df)


write_csv(df, "daily_dowy.csv")
