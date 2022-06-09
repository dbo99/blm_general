dvs <- c("k1" , "k2","sjr_blokrhff", "outage","spill_prtcl",
         "whtwtr_rel","inflow","res_elev","res_stor","temp")
### Tile

######################################
############ BLO Kerckhoff
#######################################


{
top <- ggplot(df %>% filter(dv == dvs[3]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
  scale_fill_viridis( option = "turbo", name = "CFS (15min)") + 
  scale_y_continuous(expand = c(0.02,0.02),
                     breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                     sec.axis = dup_axis()) + labs(y =NULL) + 
  
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(from = 2003, to = 2017, by = 1),
                     labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis()) + 
  facet_wrap(~scenario, ncol = 1) 


diffbott <- ggplot(df_diff %>% filter(dv == dvs[3]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
  scale_fill_gradient2( name = "CFSdiff (15min)") + 
  scale_y_continuous(expand = c(0.02,0.02),
                     breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                     sec.axis = dup_axis()) +
  
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(from = 2003, to = 2017, by = 1),
                     labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis()) + labs(y =NULL) +  facet_wrap(~scenario, ncol = 1)



p <- top / diffbott + plot_annotation(title = paste0(dvs[3], ' - 15 min data')) + 
  plot_layout(ncol = 1, heights = c(2,1))

ggsave( "tiles_blokerkhff.jpg", dpi = 300, width = 22, height = 13, units = "in") 

}

######################################
############ k2
#######################################


{
  top <- ggplot(df %>% filter(dv == dvs[2]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_viridis( option = "turbo", name = "CFS (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) + labs(y =NULL) + 
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + 
    facet_wrap(~scenario, ncol = 1) 
  
  
  diffbott <- ggplot(df_diff %>% filter(dv == dvs[2]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_gradient2( name = "CFSdiff (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) +
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + labs(y =NULL) +  facet_wrap(~scenario, ncol = 1)
  
  
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[2], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "tiles_k2.jpg", dpi = 300, width = 22, height = 13, units = "in") 
  
}

######################################
############ outage
#######################################


{
  top <- ggplot(df %>% filter(dv == dvs[4]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_viridis( option = "turbo", name = "CFS (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) + labs(y =NULL) + 
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + 
    facet_wrap(~scenario, ncol = 1) 
  
  
  diffbott <- ggplot(df_diff %>% filter(dv == dvs[4]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_gradient2( name = "CFSdiff (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) +
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + labs(y =NULL) +  facet_wrap(~scenario, ncol = 1)
  
  
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[4], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "tiles_outage.jpg", dpi = 300, width = 22, height = 13, units = "in") 
}

######################################
############ spill
#######################################


{
  top <- ggplot(df %>% filter(dv == dvs[5]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_viridis( option = "turbo", name = "CFS (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) + labs(y =NULL) + 
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + 
    facet_wrap(~scenario, ncol = 1) 
  
  
  diffbott <- ggplot(df_diff %>% filter(dv == dvs[5]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_gradient2( name = "CFSdiff (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) +
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + labs(y =NULL) +  facet_wrap(~scenario, ncol = 1)
  
  
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[5], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "tiles_spill.jpg", dpi = 300, width = 22, height = 13, units = "in") 
}

######################################
############ whitewater
#######################################


{
  top <- ggplot(df %>% filter(dv == dvs[6]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_viridis( option = "turbo", name = "CFS (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) + labs(y =NULL) + 
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + 
    facet_wrap(~scenario, ncol = 1) 
  
  
  diffbott <- ggplot(df_diff %>% filter(dv == dvs[6]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_gradient2( name = "CFSdiff (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) +
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + labs(y =NULL) +  facet_wrap(~scenario, ncol = 1)
  
  
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[6], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "tiles_whitewater.jpg", dpi = 300, width = 22, height = 13, units = "in") 
}

######################################
############ storage
#######################################


{
  top <- ggplot(df %>% filter(dv == dvs[9]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_viridis( option = "turbo", name = "AF (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) + labs(y =NULL) + 
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + 
    facet_wrap(~scenario, ncol = 1) 
  
  
  diffbott <- ggplot(df_diff %>% filter(dv == dvs[9]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_gradient2( name = "AFdiff (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) +
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + labs(y =NULL) +  facet_wrap(~scenario, ncol = 1)
  
  
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[9], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "tiles_storage.jpg", dpi = 300, width = 22, height = 13, units = "in") 
}

######################################
############ inflow
#######################################


{
  top <- ggplot(df %>% filter(dv == dvs[7]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_viridis( option = "turbo", name = "AF (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) + labs(y =NULL) + 
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + 
    facet_wrap(~scenario, ncol = 1) 
  
  
  diffbott <- ggplot(df_diff %>% filter(dv == dvs[7]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_gradient2( name = "AFdiff (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) +
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + labs(y =NULL) +  facet_wrap(~scenario, ncol = 1)
  
  
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[7], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "tiles_inflow.jpg", dpi = 300, width = 22, height = 13, units = "in") 
}

######################################
############ temp pulse
#######################################


{
  top <- ggplot(df %>% filter(dv == dvs[10]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_viridis( option = "turbo", name = "cfs (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) + labs(y =NULL) + 
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + 
    facet_wrap(~scenario, ncol = 1) 
  
  
  diffbott <- ggplot(df_diff %>% filter(dv == dvs[10]), aes(x=wy, y = -dowy_hr,  fill = value)) + geom_tile() +
    scale_fill_gradient2( name = "cfs (15min)") + 
    scale_y_continuous(expand = c(0.02,0.02),
                       breaks = c(-1,-32,-62,-93,-124,-152,-183,-213,-244,-274,-305,-336) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S"),
                       sec.axis = dup_axis()) +
    
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis()) + labs(y =NULL) +  facet_wrap(~scenario, ncol = 1)
  
  
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[10], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "tiles_temperaturepulse.jpg", dpi = 300, width = 22, height = 13, units = "in") 
}
