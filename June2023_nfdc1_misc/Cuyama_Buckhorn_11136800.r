



#{
rm(list = ls())
rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(ggridges)
library(viridis)
library(gghighlight)
library(plotly)
library(ggdark)
source("fun_defs.r")
source("create_daily_dowy.r")


daily_dowy <- read_csv("daily_dowy.csv")
#library(ggsn)
#library(sf)
#library(maps)
library(dataRetrieval)

#}
  
  
# Cuyama R BL Buckhorn Cyn NR Santa Maria CA
siteNumber <- "11136800"
#NFDC1_info <- readNWISsite(siteNumber)
parameterCd <- "00060"


## whole timeseries keeps stalling out
#startDate <- "1987-10-01"
#endDate <- "2023-06-05"
#dischargeUnit_WY88__5Jun2023 <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
#

## ten year chunks to paste together later (don)



startDate <- "1988-10-01"
endDate <- "2000-09-30"
df_a <- readNWISuv(siteNumber, parameterCd, startDate, endDate, tz = "America/Los_Angeles")

startDate <- "2000-10-01"
endDate <- "2009-09-30"
df_b <- readNWISuv(siteNumber, parameterCd, startDate, endDate, tz = "America/Los_Angeles")

startDate <- "2009-10-01"
endDate <- "2019-09-30"
df_c <- readNWISuv(siteNumber, parameterCd, startDate, endDate, tz = "America/Los_Angeles")

startDate <- "2019-10-01"
endDate <- "2023-08-14"
df_d <- readNWISuv(siteNumber, parameterCd, startDate, endDate, tz = "America/Los_Angeles")

df <- rbind(df_a, df_b, df_c, df_d)

df <- renameNWISColumns(df)


########################################################
##################################
##########################################################


df <-   df %>% transmute(usgsid = siteNumber, dtmn = dateTime, cfs = Flow_Inst, Flow_Inst_cd, tz_cd) %>% mutate(
        wy = water_year(dtmn), wm = water_month(dtmn), hour = hour(dtmn), minute = minute(dtmn)) 

as_tibble(df)
#df <- df %>% filter(wy >= 1991) # before wy 1990 the data are not on the 15s and are irregular
as_tibble(df)

df_gauge <- df

minute <- seq(0,59, b = 1)
hourfrac <- minute/60
minfractab <- data.frame(minute,hourfrac)
head(minfractab)

df <- right_join(df, minfractab)
as_tibble(df)

df <- df %>% mutate(hour = as.double(hour) + hourfrac, date = date(dtmn))
as_tibble(df)


df <- inner_join(df, daily_dowy)
as_tibble(df)

df <- df %>% mutate(dayfrac = hour/24)
as_tibble(df)
tail(df)

df <- df %>% mutate(dowy_dcml =  dowy + dayfrac)
as_tibble(df)
tail(df)



# df_hrlyqntl  <- create_hrly_quants(df, dowy_dcml, cfs)  create_hrly_quants() is broken - fix in fun_defs.r


##################################
################ plot since April 1#############
#################################


#df_p <- df %>% filter(wm >=9, wm <=11, wy != 1998) #1998 has clearly bad data that changes plot's aesthetics!
df_p <- df #%>% filter(dowy >= 244, dowy <= 336, wy != 1998) #1998 has clearly bad data that changes plot's aesthetics!
#df_p <- df %>% filter(dowy >= 183, dowy <= 336, wy != 1998) #1998 has clearly bad data that changes plot's aesthetics!
#as_tibble(df_p)


df_qntls_h <- df_p  %>% group_by(dowy_dcml) %>%
  summarize(
    `5%`=quantile(cfs,  probs=0.05, type = 6), 
    `10%`=quantile(cfs, probs=0.10, type = 6),
    `25%`=quantile(cfs, probs=0.25, type = 6),
    `50%`=quantile(cfs, probs=0.5 , type = 6), 
    `75%`=quantile(cfs, probs=0.75, type = 6),
    `90%`=quantile(cfs, probs=0.9 , type = 6),
    `95%`=quantile(cfs, probs=0.95, type = 6),
    `min`=min(cfs), `max`=max(cfs), mean=mean(cfs), median=median(cfs), NA.RM = TRUE,
    n=n())      %>%
    #delete "irregular" times other than on the 15s - at least 20 yrs of same snapshot
    filter(n > 20) 

# get current year as bold plot above spaghetti
#df_p_23 <- df_p %>% filter(wy == 2023)
#df_p_17 <- df_p %>% filter(wy == 2017)
#df_p_11 <- df_p %>% filter(wy == 2011)

#ggplot() + 
  #geom_line(data = df_p, aes(x = dowy_dcml, y = cfs, group = wy, color = 'individual\nensemble\ntrace ("year")') , linewidth = 0.2, alpha = 0.4) 
#geom_line(data = df_p, aes(x = dowy_dcml, y = cfs, group = wy, color = wy) , linewidth = 0.2, alpha = 0.4) 


pribbon <-  ggplot() + 
  
  
  geom_line(data = df_p, aes(x = dowy_dcml, y = cfs, group = wy, color = "1988-2023") , linewidth = 0.2, alpha = 0.4) +
  geom_line  (data = df_qntls_h,  aes(x=dowy_dcml, y=median, linetype = "median"), color = "blue") +
  
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `95%`, ymax = max,  fill= "95 - max"   ),  alpha=0.2) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `90%`, ymax = `95%`,fill= "90 - 95"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `75%`, ymax = `90%`,fill= "75 - 90"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `50%`, ymax = `75%`,fill= "50 - 75"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `25%`, ymax = `50%`,fill= "25 - 50"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `10%`, ymax = `25%`,fill= "10 - 25"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `5%`,  ymax = `10%`,fill= "5 - 10"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `min`, ymax = `5%`,fill=  "min to 5"  ), alpha=0.2, show.legend = FALSE) +
  
  # showlegendfalse above is for matching legend alpha to plot alpha - https://github.com/tidyverse/ggplot2/issues/2529
  
   geom_line  (data = df_p_23,     aes(x=dowy_dcml, y=cfs, color = '2023')) +
  # geom_point  (data = df_p_23,     aes(x=dowy_dcml, y=cfs, color = "2023")) +
  
  #geom_line  (data = df_p_23,     aes(x=dowy_dcml, y=cfs, color = "2017")) +
 # geom_point  (data = df_p_23,     aes(x=dowy_dcml, y=cfs, color = "2017")) +
  
  geom_line  (data = df_p_17,     aes(x=dowy_dcml, y=cfs, color = "2017 (very wet)")) +
  geom_line  (data = df_p_11,     aes(x=dowy_dcml, y=cfs, color = "2011 (very wet)")) +
  
  
  scale_fill_manual(#guide = guide_legend(override.aes = list(alpha = 0.01) ),
                    
                    values = c(
   "95 - max"    = 'snow4',
   "90 - 95"     = 'yellow',
   "75 - 90"     = 'green', 
   "50 - 75"     = 'blue',
    'median'       = 'blue4',
     "25 - 50"    = 'blue',
     "10 - 25"    = 'green',
     "5 - 10"     = 'yellow',
   "min to 5"  = 'snow4'),
                   breaks = c(
       "95 - max"     ,
       "90 - 95"      ,
       "75 - 90"      , 
       "50 - 75"      ,
        'median'      ,
         "25 - 50"    ,
         "10 - 25"     ,
         "5 - 10"       ,
       "min to 5"        )) +
  
  scale_color_manual(name = 'year traces\n(15-min raw USGS data.\n1998 excluded for missing data)', 
                     values= c( '2023' = 'red',
                                 '1990-2022' = 'gray', '2017 (very wet)' = 'orange', '2011 (very wet)' = 'white'),
      guide = guide_legend(override.aes = list(
      linetype = c("solid",    "solid", "solid", 'solid'),
      shape = c( 16, NA, NA, NA)))) +
  
  
  labs(y = "cubic feet per second", x = NULL, size = 9) +
  guides(fill=guide_legend(title="flow percentile\n(baseline 1990-2022)", fill = guide_legend(override.aes = list(alpha = 0.5)))) +
  #guides(color=guide_legend(title="series")) + 
  scale_linetype_manual(name = "",values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue")))) +
  scale_y_continuous(expand= c(0.02, 0.02)) + 
  scale_x_continuous(expand= c(0.02, 0.02), breaks = c(183, 213, 244, 251, 274, 305, 336 ), labels = c("April 1", "May 1",  "June 1", "June 9", "July 1", "Aug 1", "Sep 1")) +
  theme(plot.margin = unit(c(1,1,1,1), "in")) +  theme(legend.position = c(0.5, 0.2)) +
  ggtitle("Scott R NR Fort Jones CA - 11519500") + dark_theme_gray() #+
  #guides(color = guide_legend(override.aes = list(alpha = 1)))

pribbon

#z <- ggplotly(pribbon)
#htmlwidgets::saveWidget(as_widget(z), "nf_auburn_ribbons_apr1.html")
ggsave("Scott R NR Fort Jones CA - 11519500.png", height = 10, width = 27, units = "in", dpi = 300)


##############################
###################  since June 1
###############################

##################################
################ plot #############
#################################


#df_p <- df %>% filter(wm >=9, wm <=11, wy != 1998) #1998 has clearly bad data that changes plot's aesthetics!
#df_p <- df %>% filter(dowy >= 244, dowy <= 336, wy != 1998) #1998 has clearly bad data that changes plot's aesthetics!
df_p <- df %>% filter(dowy >= 244, dowy <= 336, wy != 1998) #1998 has clearly bad data that changes plot's aesthetics!
as_tibble(df_p)


df_qntls_h <- df_p  %>% group_by(dowy_dcml) %>%
  summarize(
    `5%`=quantile(cfs,  probs=0.05, type = 6), 
    `10%`=quantile(cfs, probs=0.10, type = 6),
    `25%`=quantile(cfs, probs=0.25, type = 6),
    `50%`=quantile(cfs, probs=0.5 , type = 6), 
    `75%`=quantile(cfs, probs=0.75, type = 6),
    `90%`=quantile(cfs, probs=0.9 , type = 6),
    `95%`=quantile(cfs, probs=0.95, type = 6),
    `min`=min(cfs), `max`=max(cfs), mean=mean(cfs), median=median(cfs), 
    n=n())      %>%
  #delete "irregular" times other than on the 15s - at least 20 yrs of same snapshot
  filter(n > 20) 

# get current year as bold plot above spaghetti
df_p_23 <- df_p %>% filter(wy == 2023)
df_p_17 <- df_p %>% filter(wy == 2017)
df_p_11 <- df_p %>% filter(wy == 2011)

#ggplot() + 
#geom_line(data = df_p, aes(x = dowy_dcml, y = cfs, group = wy, color = 'individual\nensemble\ntrace ("year")') , linewidth = 0.2, alpha = 0.4) 
#geom_line(data = df_p, aes(x = dowy_dcml, y = cfs, group = wy, color = wy) , linewidth = 0.2, alpha = 0.4) 


pribbon <-  ggplot() + 
  
  
  geom_line(data = df_p, aes(x = dowy_dcml, y = cfs, group = wy, color = "1990-2022") , linewidth = 0.2, alpha = 0.4) +
  geom_line  (data = df_qntls_h,  aes(x=dowy_dcml, y=median, linetype = "median"), color = "blue") +
  
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `95%`, ymax = max,  fill= "95 - max"   ),  alpha=0.2) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `90%`, ymax = `95%`,fill= "90 - 95"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `75%`, ymax = `90%`,fill= "75 - 90"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `50%`, ymax = `75%`,fill= "50 - 75"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `25%`, ymax = `50%`,fill= "25 - 50"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `10%`, ymax = `25%`,fill= "10 - 25"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `5%`,  ymax = `10%`,fill= "5 - 10"   ),   alpha=0.2, show.legend = FALSE) +
  geom_ribbon(data = df_qntls_h,  aes(x=dowy_dcml, ymin= `min`, ymax = `5%`,fill=  "min to 5"  ), alpha=0.2, show.legend = FALSE) +
  
  # showlegendfalse above is for matching legend alpha to plot alpha - https://github.com/tidyverse/ggplot2/issues/2529
  

  # geom_point  (data = df_p_23,     aes(x=dowy_dcml, y=cfs, color = "2023")) +
  
  #geom_line  (data = df_p_23,     aes(x=dowy_dcml, y=cfs, color = "2017")) +
  # geom_point  (data = df_p_23,     aes(x=dowy_dcml, y=cfs, color = "2017")) +
  
  geom_line  (data = df_p_17,     aes(x=dowy_dcml, y=cfs, color = "2017 (very wet)")) +

  geom_line  (data = df_p_23,     aes(x=dowy_dcml, y=cfs, color = '2023')) +
  geom_line  (data = df_p_11,     aes(x=dowy_dcml, y=cfs, color = "2011 (very wet)")) +
  
  scale_fill_manual(#guide = guide_legend(override.aes = list(alpha = 0.01) ),
    
    values = c(
      "95 - max"    = 'snow4',
      "90 - 95"     = 'yellow',
      "75 - 90"     = 'green', 
      "50 - 75"     = 'blue',
      'median'       = 'blue4',
      "25 - 50"    = 'blue',
      "10 - 25"    = 'green',
      "5 - 10"     = 'yellow',
      "min to 5"  = 'snow4'),
    breaks = c(
      "95 - max"     ,
      "90 - 95"      ,
      "75 - 90"      , 
      "50 - 75"      ,
      'median'      ,
      "25 - 50"    ,
      "10 - 25"     ,
      "5 - 10"       ,
      "min to 5"        )) +
  
  scale_color_manual(name = 'year traces\n(15-min raw USGS data.\n1998 excluded for missing data)', 
                     values= c( '2023' = 'red',
                                '1990-2022' = 'gray', '2017 (very wet)' = 'orange', '2011 (very wet)' = 'white'),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid",    "solid", "solid", 'solid'),
                       shape = c( 16, NA, NA, NA)))) +
  
  
  labs(y = "cubic feet per second", x = NULL, size = 9) +
  guides(fill=guide_legend(title="flow percentile\n(baseline 1990-2022)", fill = guide_legend(override.aes = list(alpha = 0.5)))) +
  #guides(color=guide_legend(title="series")) + 
  scale_linetype_manual(name = "",values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue")))) +
  scale_y_continuous(expand= c(0.02, 0.02)) + 
  scale_x_continuous(expand= c(0.02, 0.02), breaks = c(244, 251, 274, 305, 336 ), labels = c( "6/1", "6/9", "July 1", "Aug 1", "Sep 1")) +
  theme(plot.margin = unit(c(1,1,1,1), "in")) +  theme(legend.position = c(0.5, 0.2)) +
  ggtitle("North Fork American River flow, Auburn, Ca (1990-2023) (unregulated)\nJune 1 toward baseflow\n2023 Conditions (red) relative to 1990-2022") + dark_theme_gray() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12))

pribbon

#z <- ggplotly(pribbon)
#htmlwidgets::saveWidget(as_widget(z), "nf_auburn_ribbons_apr1.html")
ggsave("nfrkamrcn_June 1.png", height = 8, width = 9.5, units = "in", dpi = 300)
