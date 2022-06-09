##########################
#######  original (before it was a loop)
######################
#p_excd_wy_and_wytype_eachmonyr <- function(df) {
#  
#  df   %>% #filter( wm == 7, wy == 2011) %>%  ##don't forget to remove
#    #df %>% filter(dv  == "temp", wy >= 2009, wy <= 2013) %>%  ##don't forget to remove
#    group_by(scenario, dv, wy_sjwytf) %>% arrange(dv, desc(value)) %>% 
#    mutate(rank = row_number(),
#           excdxaxis = rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = value, color = scenario, linetype = scenario)) +
#    geom_line() + labs(x = "probability of exceedance", y = "value -- 15 min per avg")+ #theme_gray() +
#    #guides(colour = guide_legend(override.aes = list(size=2))) + 
#    theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
#    #scale_color_manual(values = df_cols) +
#    facet_wrap(wy_sjwytf~dv, scales = "free_y", nrow =5) + #theme(strip.text.y.left = element_text(angle = 0)) +
#    ggtitle(paste0(unique(df$wmt),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
#    scale_x_continuous(expand = c(0.005,0.005),
#                       breaks = seq(from = 0.0, to = 1.0, by = .1),
#                       labels = seq(from = 0.0, to = 1.0, by = .1),
#                       sec.axis = dup_axis(name = NULL)) +
#    
#    scale_y_continuous(expand = c(0.005,0.005))
#}
##dft <- df %>% filter(wy >=2011, wy <= 2017)
#p_excd_wy_and_wytype_eachmonyr(df)
#
#ggsave( "excd.jpg", dpi = 300, width = 22, height = 13, units = "in") 

##########################
#######   168 months
######################


p_excd_wy_and_wytype_eachmonyr <- function(df) {
  
  df   %>% #filter( wm == 7, wy == 2011) %>%  ##don't forget to remove
    #df %>% filter(dv  == "temp", wy >= 2009, wy <= 2013) %>%  ##don't forget to remove
    group_by(scenario, dv, mon_yr_wy) %>% arrange(dv, desc(value)) %>% 
    mutate(rank = row_number(),
           excdxaxis = rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = value, color = scenario, linetype = scenario)) +
    geom_line() + labs(x = "probability of exceedance", y = "value -- 15 min per avg")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    #scale_color_manual(values = df_cols) +
    facet_wrap(~dv, scales = "free_y", nrow =5) + #theme(strip.text.y.left = element_text(angle = 0)) +
    #ggtitle(paste0(unique(df$wmt),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    scale_x_continuous(expand = c(0.005,0.005),
                       breaks = seq(from = 0.0, to = 1.0, by = .1),
                       labels = seq(from = 0.0, to = 1.0, by = .1),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0.005,0.005))
}


#p <- p_excd_wy_and_wytype_eachmonyr(df)

#ggsave("excd.jpg", dpi = 300, width = 22, height = 13, units = "in") 

######

mons <- unique(df$mon_yr_wy)
lengmons <- seq(from = 1, to = length(mons), by = 1)

for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_excd_wy_and_wytype_eachmonyr(df %>% filter(mon_yr_wy == mons[i])) +
    ggtitle(paste0(mons[i]))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                      ".jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}

##########################
#######  60 mons & year types
######################


p_excd_wy_and_wytype_monwy <- function(df) {
  
  df   %>% #filter( wm == 7, wy == 2011) %>%  ##don't forget to remove
    #df %>% filter(dv  == "temp", wy >= 2009, wy <= 2013) %>%  ##don't forget to remove
    group_by(scenario, dv, wmt_sjwytf) %>% arrange(dv, desc(value)) %>% 
    mutate(rank = row_number(),
           excdxaxis = rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = value, color = scenario, linetype = scenario)) +
    geom_line() + labs(x = "probability of exceedance", y = "value -- 15 min per avg")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    #scale_color_manual(values = df_cols) +
    facet_wrap(~dv, scales = "free_y", nrow =5) + #theme(strip.text.y.left = element_text(angle = 0)) +
    ggtitle(paste0(unique(df$wmt),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    scale_x_continuous(expand = c(0.005,0.005),
                       breaks = seq(from = 0.0, to = 1.0, by = .1),
                       labels = seq(from = 0.0, to = 1.0, by = .1),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0.005,0.005))
}


#p <- p_excd_wy_and_wytype_eachmonyr(df)

#ggsave("excd.jpg", dpi = 300, width = 22, height = 13, units = "in") 

######

mons <- unique(df$wmt_sjwytf)
lengmons <- seq(from = 1, to = length(mons), by = 1)

for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_excd_wy_and_wytype_monwy(df %>% filter(wmt_sjwytf == mons[i])) +
    ggtitle(paste0(mons[i], " (all)"))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                ".jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}


##########################
#######  12 mons (makes 12 plots)
######################


p_excd_wy_and_wytype_12mons <- function(df) {
  
  df   %>% #filter( wm == 7, wy == 2011) %>%  ##don't forget to remove
    #df %>% filter(dv  == "temp", wy >= 2009, wy <= 2013) %>%  ##don't forget to remove
    group_by(scenario, dv, wm) %>% arrange(dv, desc(value)) %>% 
    mutate(rank = row_number(),
           excdxaxis = rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = value, color = scenario, linetype = scenario)) +
    geom_line() + labs(x = "probability of exceedance", y = "value -- 15 min per avg")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    #scale_color_manual(values = df_cols) +
    facet_wrap(~dv, scales = "free_y", nrow =5) + #theme(strip.text.y.left = element_text(angle = 0)) +
    ggtitle(paste0(unique(df$wmt),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    scale_x_continuous(expand = c(0.005,0.005),
                       breaks = seq(from = 0.0, to = 1.0, by = .1),
                       labels = seq(from = 0.0, to = 1.0, by = .1),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0.005,0.005))
}


#p <- p_excd_wy_and_wytype_eachmonyr(df)

#ggsave("excd.jpg", dpi = 300, width = 22, height = 13, units = "in") 

######

mons <- unique(df$wm)
lengmons <- seq(from = 1, to = length(mons), by = 1)

for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_excd_wy_and_wytype_12mons(df %>% filter(wm == mons[i])) +
    ggtitle(paste0(wmt[i], " (all)"))
  ggsave(paste0(lengmons[i], " - ", wmt[i],
                ".jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}


##########################
#######  by DV & month (10 plots)
######################
dvs <- c("k1" , "k2","sjr_blokrhff", "outage","spill_prtcl",
         "whtwtr_rel","inflow","res_elev","res_stor","temp")

p_excd_wy_and_wytype_byDVmonth <- function(df) {
  
  df   %>% #filter( wm == 7, wy == 2011) %>%  ##don't forget to remove
    #df %>% filter(dv  == "temp", wy >= 2009, wy <= 2013) %>%  ##don't forget to remove
    group_by(scenario, dv, wm) %>% arrange(dv, desc(value)) %>% 
    mutate(rank = row_number(),
           excdxaxis = rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = value, color = scenario, linetype = scenario)) +
    geom_line() + labs(x = "probability of exceedance", y = "value -- 15 min per avg")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    #scale_color_manual(values = df_cols) +
    facet_wrap(~wmt, scales = "free_y", nrow =6) + #theme(strip.text.y.left = element_text(angle = 0)) +
    #ggtitle(paste0(unique(df$dv),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    scale_x_continuous(expand = c(0.005,0.005),
                       breaks = seq(from = 0.0, to = 1.0, by = .1),
                       labels = seq(from = 0.0, to = 1.0, by = .1),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0.005,0.005))
}


#p <- p_excd_wy_and_wytype_eachmonyr(df)

#ggsave("excd.jpg", dpi = 300, width = 22, height = 13, units = "in") 

######

mons <- unique(df$dv)
lengmons <- seq(from = 1, to = length(mons), by = 1)

for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_excd_wy_and_wytype_byDVmonth(df %>% filter(dv == dvs[i])) +
    ggtitle(paste0(dvs[i], " (all)"))
  ggsave(paste0(lengmons[i], " - ", dvs[i],
                ".jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}


##########################
#######  by DV & water year type (50 plots)
######################
dvs <- c("k1" , "k2","sjr_blokrhff", "outage","spill_prtcl",
         "whtwtr_rel","inflow","res_elev","res_stor","temp")

p_excd_wy_and_wytype_byDVmonthwyt <- function(df) {
  
  df   %>% #filter( wm == 7, wy == 2011) %>%  ##don't forget to remove
    #df %>% filter(dv  == "temp", wy >= 2009, wy <= 2013) %>%  ##don't forget to remove
    group_by(scenario, dv, wmt_sjwytf) %>% arrange(dv, desc(value)) %>% 
    mutate(rank = row_number(),
           excdxaxis = rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = value, color = scenario, linetype = scenario)) +
    geom_line() + labs(x = "probability of exceedance", y = "value -- 15 min per avg")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    #scale_color_manual(values = df_cols) +
    facet_wrap(~wmt, scales = "free_y", nrow =6) + #theme(strip.text.y.left = element_text(angle = 0)) +
    #ggtitle(paste0(unique(df$dv),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    scale_x_continuous(expand = c(0.005,0.005),
                       breaks = seq(from = 0.0, to = 1.0, by = .1),
                       labels = seq(from = 0.0, to = 1.0, by = .1),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0.005,0.005))
}


#p <- p_excd_wy_and_wytype_eachmonyr(df)

#ggsave("excd.jpg", dpi = 300, width = 22, height = 13, units = "in") 

######

mons <- unique(df$dv_sjwytf)
lengmons <- seq(from = 1, to = length(mons), by = 1)

for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_excd_wy_and_wytype_byDVmonthwyt(df %>% filter(dv_sjwytf == mons[i])) +
    ggtitle(paste0(mons[i]))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                ".jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}


##########################
#######  by DV & monyear  (140 plots)
######################
dvs <- c("k1" , "k2","sjr_blokrhff", "outage","spill_prtcl",
         "whtwtr_rel","inflow","res_elev","res_stor","temp")

p_excd_wy_and_wytype_byDVmonthwyt_wy <- function(df) {
  
  df   %>% #filter( wm == 7, wy == 2011) %>%  ##don't forget to remove
    #df %>% filter(dv  == "temp", wy >= 2009, wy <= 2013) %>%  ##don't forget to remove
    group_by(scenario, wmt, dv_wy_sjwytf) %>% arrange(dv, desc(value)) %>% 
    mutate(rank = row_number(),
           excdxaxis = rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = value, color = scenario, linetype = scenario)) +
    geom_line() + labs(x = "probability of exceedance", y = "value -- 15 min per avg")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
    #scale_color_manual(values = df_cols) +
    facet_wrap(~wmt, scales = "free_y", nrow =6) + #theme(strip.text.y.left = element_text(angle = 0)) +
    #ggtitle(paste0(unique(df$dv),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    scale_x_continuous(expand = c(0.005,0.005),
                       breaks = seq(from = 0.0, to = 1.0, by = .1),
                       labels = seq(from = 0.0, to = 1.0, by = .1),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0.005,0.005))
}


#p <- p_excd_wy_and_wytype_eachmonyr(df)

#ggsave("excd.jpg", dpi = 300, width = 22, height = 13, units = "in") 

######

mons <- unique(df$dv_wy_sjwytf)
lengmons <- seq(from = 1, to = length(mons), by = 1)

for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_excd_wy_and_wytype_byDVmonthwyt_wy(df %>% filter(dv_wy_sjwytf == mons[i])) +
    ggtitle(paste0(mons[i]))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                ".jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}
mons
