
rm(list = ls())

{

  library(tidyverse)
  library(lubridate)
  library(ggridges)
  library(viridis)
  library(zoo)
  library(ggplotly)
  rstudioapi::getActiveDocumentContext
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) }

  #source("fun_defs.r")
  #daily_dowy <- read_csv("daily_dowy.csv")  


{
piezpos <- read_csv("piezpos.csv")
head(piezpos)
cs1 <- read_csv("r1.csv")     %>% mutate(piezo_nn = "cs1"  )
cs2 <- read_csv("r2.csv")     %>% mutate(piezo_nn = "cs2"  )
cs3 <- read_csv("r3.csv")     %>% mutate(piezo_nn = "cs3"  )
cs4 <- read_csv("r4.csv")     %>% mutate(piezo_nn = "cs4"  )
cs5 <- read_csv("r5.csv")     %>% mutate(piezo_nn = "cs5"  )
cs6 <- read_csv("r6.csv")     %>% mutate(piezo_nn = "cs6"  )
cs7 <- read_csv("r7.csv")     %>% mutate(piezo_nn = "cs7"  )
cs8 <- read_csv("r8.csv")     %>% mutate(piezo_nn = "cs8"  )
cs9 <- read_csv("r9.csv")     %>% mutate(piezo_nn = "cs9"  )
cs10 <- read_csv("r10.csv")   %>% mutate(piezo_nn = "cs10" )
cs11 <- read_csv("r11.csv")   %>% mutate(piezo_nn = "cs11" )
csdvj <- read_csv("rDVJ.csv") %>% mutate(piezo_nn = "dvj")

pz_ts <- rbind(cs1, cs2, cs3, cs4, cs5, cs6, cs7, cs8, cs9, cs10, cs11,  csdvj)
head(pz_ts)
}
rm(list = ls()[grep("^cs", ls())]) #beware removes variables beginning with cs
head(pz_ts)  

pz_ts <- pz_ts %>% mutate(date = mdy(date), date_tm = paste0(date, " ", time)) 
head(pz_ts)
pz_ts <- pz_ts %>% mutate(date_tm = ymd_hms(date_tm))
head(pz_ts)

pz_pos_ts <- inner_join(piezpos, pz_ts)
head(pz_pos_ts)

pz_pos_ts <- pz_pos_ts %>% mutate(ft_bgs = depblotoc - stickup, elev1 = gelev1 + stickup - depblotoc,
                                  elev2 = gelev2 + stickup - depblotoc, year = year(date), yday = yday(date),
                                  bl_depchange = bl_dpblotoc - depblotoc, 
                                  feb_bl_depchange = feb_bl_dpblotoc - depblotoc,
                                  decimal_date = decimal_date(date), month = month(date))
head(pz_pos_ts)


mon_name <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
month <- seq(from =1, to = 12, by = 1)
montable <- data.frame(mon_name, month)
 

pz_pos_ts <- inner_join( pz_pos_ts, montable)

pz_pos_ts$mon_name <- factor(pz_pos_ts$mon_name, levels = 
                               c( "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec", "jan"))

pz_pos_ts$piezo_nn <- factor(pz_pos_ts$piezo_nn, levels = 
                               c( "cs1", "cs2", "cs3", "cs4", "cs5", "cs6", "cs7", "cs8", "cs9", "cs10", "cs11",  "dvj"))




#################################
############## plots
#################################
xscale_breaks = c( 2011.085, 2011.581, 
                   2012.085, 2012.581, 
                   2013.085, 2013.581,
                   2014.085, 2014.581, 
                   2015.085, 2015.581, 
                   2016.085, 2016.581,
                   2017.085, 2017.581, 
                   2018.085, 2018.581, 
                   2019.085, 2019.581, 
                   2020.085, 2020.581,
                   2021.085, 2021.581, 
                   2022.085, 2022.581, 
                             2023.085)

xscale_labels = c( "2/1/2011", "8/1/2011",
                   "2/1/2012", "8/1/2012",
                   "2/1/2013", "8/1/2013",
                   "2/1/2014", "8/1/2014",  
                   "2/1/2015", "8/1/2015",  
                   "2/1/2016", "8/1/2016",  
                   "2/1/2017", "8/1/2017",  
                   "2/1/2018", "8/1/2018",  
                   "2/1/2019", "8/1/2019",  
                   "2/1/2020", "8/1/2020",  
                   "2/1/2021", "8/1/2021",  
                   "2/1/2022", "8/1/2022",  
                               "2/1/2023")
                  
#decimal_date(mdy("2/1/2011")) - make start and end months of timeseries similar..last measurement was feb

feb_bl <- ggplot(pz_pos_ts %>% filter(decimal_date > 2011.085), aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
      #ggplot(pz_pos_ts , aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, color = "black") + scale_x_continuous(breaks = xscale_breaks, labels = xscale_labels) +   geom_hline(yintercept = 0, color = "black") + 
              geom_smooth(method = lm, se = FALSE, size = .5) + 
              geom_vline(xintercept = 2011.085, color = "red") +
              geom_vline(xintercept = 2023.085, color = "red") #+ theme(axis.text.x = element_text(angle = 90))
feb_bl
ggsave("feb11baseline.pdf", dpi = 300, width = 25, height = 12, units = "in") 

wtabl <- ggplot(pz_pos_ts, aes(x = decimal_date, y = elev2, color = piezo_nn)) + geom_point() + 
  #facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  #ggplot(pz_pos_ts , aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = xscale_breaks, labels = xscale_labels) +   
  #geom_smooth(method = lm, se = FALSE, size = .5) + 
  geom_vline(xintercept = 2011.085, color = "red") + geom_line(size = 0.25) +
  geom_vline(xintercept = 2023.085, color = "red") #+ theme(axis.text.x = element_text(angle = 90))
wtabl
ggsave("watertable.pdf", dpi = 300, width = 25, height = 12, units = "in") 


ft_bgs_free <- ggplot(pz_pos_ts, aes(x = decimal_date, y = ft_bgs*(-1), color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  #ggplot(pz_pos_ts , aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, color = "black") + scale_x_continuous(breaks = xscale_breaks, labels = xscale_labels) +   geom_hline(yintercept = 0, color = "black") + 
  #geom_smooth(method = lm, se = FALSE, size = .5) + 
  geom_vline(xintercept = 2011.085, color = "red") +
  geom_vline(xintercept = 2023.085, color = "red") #+ theme(axis.text.x = element_text(angle = 90))
ft_bgs_free
ggsave("ft_bgs_free.pdf", dpi = 300, width = 25, height = 12, units = "in") 

ft_bgs <- ggplot(pz_pos_ts, aes(x = decimal_date, y = ft_bgs*(-1), color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1) +
  #ggplot(pz_pos_ts , aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, color = "black") + scale_x_continuous(breaks = xscale_breaks, labels = xscale_labels) +   geom_hline(yintercept = 0, color = "black") + 
  #geom_smooth(method = lm, se = FALSE, size = .5) + 
  geom_vline(xintercept = 2011.085, color = "red") +
  geom_vline(xintercept = 2023.085, color = "red") #+ theme(axis.text.x = element_text(angle = 90))
ft_bgs
ggsave("ft_bgs.pdf", dpi = 300, width = 25, height = 12, units = "in") 


monthboxes <- ggplot(pz_pos_ts, aes(mon_name, y = ft_bgs*(-1), group = month, color = piezo_nn)) + geom_boxplot() + facet_wrap(~piezo_nn)
monthboxes
ggsave("mon_boxes.pdf", dpi = 300, width = 25, height = 12, units = "in") 
