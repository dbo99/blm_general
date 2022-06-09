p <- ggplot(df %>% filter(dv == "sjr_blokrhff"), aes(date_time, value, color = scenario, linetype = scenario)) + geom_line()
p
z <- ggplotly(p)
htmlwidgets::saveWidget(as_widget(z), "blokrkhff2.html")

## 
