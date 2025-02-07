dates <- seq.Date(mdy("1/1/1930"), mdy("9/30/2040"), by = "1 day")
yday <- yday(dates)





forexcel <- data.frame(dates, yday)
write_csv(forexcel, 'forexcel.csv')
