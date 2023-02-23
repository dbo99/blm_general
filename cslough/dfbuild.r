
rm(list = ls())

{

  library(tidyverse)
  library(lubridate)
  #library(ggridges)
  #library(viridis)
  #library(zoo)
  library(plotly)
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

#pz_pos_ts$piezo_nn <- factor(pz_pos_ts$piezo_nn, levels = 
#                               c( "cs1", "cs2", "cs3", "cs4", "cs5", "cs6", "cs7", "cs8", "cs9", "cs10", "cs11",  "dvj"))

pz_pos_ts$piezo_nn <- factor(pz_pos_ts$piezo_nn, levels = 
                               c( "cs6", "cs7","cs5", "cs8", "cs4", "cs3", "cs2",  "cs1", "cs9", "cs10", "dvj",  "cs11"))


#pz_pos_ts <- pz_pos_ts %>% arrange(ymd(pz_pos_ts$date))

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

############################
########## changes relative to feb 11 baseline
###############################



{
feb_bl <- ggplot(pz_pos_ts %>% filter(decimal_date > 2011.085), aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
      #ggplot(pz_pos_ts , aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, color = "black") + scale_x_continuous(breaks = xscale_breaks, labels = xscale_labels) +   geom_hline(yintercept = 0, color = "black") + 
              geom_smooth(method = lm, se = FALSE, size = .5) + 
              geom_vline(xintercept = 2011.085, color = "red") +
              geom_vline(xintercept = 2023.085, color = "red")  + #+ theme(axis.text.x = element_text(angle = 90))
              theme(legend.position = "none") + labs(title = "Carson Slough BLM Piezometers", 
                                                     subtitle = "water table change relative to baseline data (Feb '11). Plots ordered generally upgradient to downgradient.(free y-axis)", 
                                                     caption = "Feb 2023. Transducer data or manual measurements pre-Feb '11 not included.") +
              labs(x = "date", y = "change in feet relative to February 2011 (or closest available) measurement") + 
              scale_y_continuous(sec.axis = dup_axis(name = NULL))
feb_bl
ggsave("timeseries_baseline_diff_free_y.pdf", dpi = 300, width = 17, height = 11, units = "in") }

{
feb_bl_fxd <- ggplot(pz_pos_ts %>% filter(decimal_date > 2011.085), aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + 
  geom_point() + facet_wrap(~piezo_nn, ncol = 1) +
  #ggplot(pz_pos_ts , aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, color = "black") + scale_x_continuous(breaks = xscale_breaks, labels = xscale_labels) +   
  geom_hline(yintercept = 0, color = "black") + 
  geom_smooth(method = lm, se = FALSE, size = .5) + 
  geom_vline(xintercept = 2011.085, color = "red") +
  geom_vline(xintercept = 2023.085, color = "red")  + #+ theme(axis.text.x = element_text(angle = 90))
  theme(legend.position = "none") + labs(title = "Carson Slough BLM Piezometers", 
                                         subtitle = "water table change relative to baseline data (Feb '11). Plots ordered generally upgradient to downgradient. (fixed y-axis)", 
                                         caption = "Feb 2023. Transducer data or manual measurements pre-Feb '11 not included. ") +
  labs(x = "date", y = "change in feet relative to February 2011 (or closest available) measurement") + 
  scale_y_continuous(sec.axis = dup_axis(name = NULL))
feb_bl_fxd
ggsave("timeseries_baseline_diff_fixed_y.pdf", dpi = 300, width = 17, height = 11, units = "in") }

############################
########## water table
###############################


{
wtabl <- ggplot(pz_pos_ts, aes(x = decimal_date, y = elev2, color = piezo_nn)) + geom_point() + 
  #facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  #ggplot(pz_pos_ts , aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = xscale_breaks, labels = xscale_labels) +   
  #geom_smooth(method = lm, se = FALSE, size = .5) + 
  geom_vline(xintercept = 2011.085, color = "red") + geom_line(size = 0.25) +
  geom_vline(xintercept = 2023.085, color = "red") + #+ theme(axis.text.x = element_text(angle = 90))
  labs(title = "Carson Slough BLM Piezometers", 
         subtitle = "timeseries of approximate groundwater elevation (using June '10 survey data)", 
         caption = "Feb 2023. Transducer data not included.") +
         labs(x = "date", y = "groundwater elevation (feet above mean sea level)") +
         guides(color = guide_legend(title = "piez.")) + 
        scale_y_continuous(breaks = c(2034, 2036, 2038, 2040, 2042, 2044, 2046, 2048, 2050, 2052, 2054, 2056 ),
                           sec.axis = dup_axis(name = NULL))
wtabl
ggsave("timeseries_elevation.pdf", dpi = 300, width = 17, height = 11, units = "in") }


############################
########## ft bgs
###############################

{
ft_bgs_free <- ggplot(pz_pos_ts, aes(x = decimal_date, y = ft_bgs*(-1), color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  #ggplot(pz_pos_ts , aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, color = "black") + scale_x_continuous(breaks = xscale_breaks, labels = xscale_labels) +   geom_hline(yintercept = 0, color = "black") + 
  #geom_smooth(method = lm, se = FALSE, size = .5) + 
  geom_vline(xintercept = 2011.085, color = "red") +
  geom_vline(xintercept = 2023.085, color = "red") + #+ theme(axis.text.x = element_text(angle = 90))
  labs(title = "Carson Slough BLM Piezometers", 
       subtitle = "timeseries of groundwater depth, free y-axis", 
       caption = "Feb 2023. Transducer data not included.") +
  labs(x = "date", y = "feet relative to ground surface") +  scale_y_continuous(sec.axis = dup_axis(name = NULL))
ft_bgs_free
ggsave("timeseries_depth_free_y.pdf", dpi = 300, width = 17, height = 11, units = "in") }


{
ft_bgs <- ggplot(pz_pos_ts, aes(x = decimal_date, y = ft_bgs*(-1), color = piezo_nn)) + geom_point() + 
  facet_wrap(~piezo_nn, ncol = 1) +
  #ggplot(pz_pos_ts , aes(x = decimal_date, y = feb_bl_depchange, color = piezo_nn)) + geom_point() + facet_wrap(~piezo_nn, ncol = 1, scales = "free_y") +
  geom_hline(yintercept = 0, color = "black") + scale_x_continuous(breaks = xscale_breaks, labels = xscale_labels) +   geom_hline(yintercept = 0, color = "black") + 
  #geom_smooth(method = lm, se = FALSE, size = .5) + 
  geom_vline(xintercept = 2011.085, color = "red") +
  geom_vline(xintercept = 2023.085, color = "red") + #+ theme(axis.text.x = element_text(angle = 90))
  labs(title = "Carson Slough BLM Piezometers", 
       subtitle = "timeseries of groundwater depth, fixed y-axis", 
       caption = "Feb 2023. Transducer data not included.") +
  labs(x = "date", y = "feet relative to ground surface") +  scale_y_continuous(sec.axis = dup_axis(name = NULL))
ft_bgs
ggsave("timeseries_depth_fixed_y.pdf", dpi = 300, width = 17, height = 11, units = "in") }


############################
########## box plots
###############################

{
monthboxes <- ggplot(pz_pos_ts, aes(mon_name, y = ft_bgs*(-1), group = month, color = piezo_nn)) + geom_boxplot() + 
              facet_wrap(~piezo_nn) +   labs(x = "month", y = "feet relative to ground surface") +
  labs(title = "Carson Slough BLM Piezometers", 
       subtitle = "box plots of groundwater depth", 
       caption = "Feb 2023. Transducer data not included.")
monthboxes
ggsave("boxplots_piezosmonthly_depth.pdf", dpi = 300, width = 17, height = 11, units = "in") }

ggplotly(monthboxes) 



{
piezboxes <- ggplot(pz_pos_ts, aes(mon_name, y = ft_bgs*(-1), group = piezo_nn, color = piezo_nn)) + geom_boxplot() + 
  facet_wrap(~piezo_nn) +   labs(x = "month", y = "feet relative to ground surface") +
  labs(title = "Carson Slough BLM Piezometers", 
       subtitle = "box plots of groundwater depth", 
       caption = "Feb 2023. Transducer data not included.")}
piezboxes
ggsave("boxplots_piezos_depth.pdf", dpi = 300, width = 17, height = 11, units = "in") 

ggplotly(piezboxes)

############################
########## plotly elev
###############################

p1 <- ggplot(pz_pos_ts, aes(date_tm, elev2, color = piezo_nn, group = piezo_nn, text = notes)) + geom_line() + geom_point() +
  labs(title = "Carson Slough BLM Piezometers, timeseries of groundwater elevation", 

       caption = "Feb 2023. Transducer data not included.") +
  labs(x = "date", y = "feet above mean sea level")  #+     guides(color = guide_legend(title = "piez.")) 
p1

ggplotly(p1, hoverinfo = c("piezo_nn", "depblotoc", "gelev2", "ft_bgs", "elev2", "notes"))
#p1ly <- ggplotly(p1, hoverinfo = c("piezo_nn", "depblotoc", "gelev2", "ft_bgs", "elev2", "notes"))
#htmlwidgets::saveWidget(as_widget(p1ly), "gw_elevation.html")

p2 <- ggplot(pz_pos_ts, aes(date_tm, ft_bgs*(-1), color = piezo_nn, group = piezo_nn, text = notes)) + geom_line() + geom_point() +
  labs(title = "Carson Slough BLM Piezometers, timeseries of groundwater depth", 
       
       caption = "Feb 2023. Transducer data not included.") + facet_wrap(~piezo_nn, ncol = 1) +
  labs(x = "date", y = "feet relative to ground surface")  #+     guides(color = guide_legend(title = "piez.")) 
p2
ggplotly(p2, hoverinfo = c("piezo_nn", "depblotoc", "gelev2", "ft_bgs", "elev2", "notes"))
#p2ly <- ggplotly(p2, hoverinfo = c("piezo_nn", "depblotoc", "gelev2", "ft_bgs", "elev2", "notes"))
#htmlwidgets::saveWidget(as_widget(p2ly), "gw_depth.html")

