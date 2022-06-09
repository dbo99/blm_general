library(plotly)
library(cowplot)
library(patchwork)
#source("df_build.r")
#dvs <- c("k1" , "k2","sjr_blokrhff", "outage","spill_prtcl",
#         "whtwtr_rel","inflow","res_elev","res_stor","temp")

#############################################
#### BLO kerckhoff #####
#############################################

#df <- df %>% mutate(wy_wt = paste0(wy," ", sjwyt))
#df_diff <- df_diff %>% mutate(wy_wt = paste0(wy," ", sjwyt))


{
ridgelinethickness <- 0.00000001
top <- ggplot(df %>% filter(dv == dvs[3]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
  geom_ridgeline_gradient(scale = 0.00015, min_height = -3500, size = ridgelinethickness) + 
  #scale_fill_gradient(colors=turbo())  +
  scale_fill_viridis(option="turbo", name = "cfs     ") +
  facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 2003, to = 2017, by = 1),
                     labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
#ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
#top

diffbott <-  ggplot(df_diff %>% filter(dv == dvs[3]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
  geom_ridgeline_gradient(scale = 0.00045, min_height = -3500, size = ridgelinethickness) + 
  #scale_fill_gradient(colors=turbo())  +
  #scale_fill_viridis(option="turbo") +
  facet_grid(~scenario) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 2003, to = 2017, by = 1),
                     labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) +   scale_fill_gradient2( name = "cfs_diff") + labs(x = NULL)

p <- top / diffbott + plot_annotation(title = paste0(dvs[3], ' - 15 min data')) + 
         plot_layout(ncol = 1, heights = c(2,1))

  ggsave( "ridges_blokerkhff.jpg", dpi = 300, width = 22, height = 13, units = "in") 

}
  #############################################
  #### Inflow  #####
  #############################################

  
  
  {
  ridgelinethickness <- 0.00000001
  top <- ggplot(df %>% filter(dv == dvs[7]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.00009, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    scale_fill_viridis(option="turbo", name = "cfs     ") +
    facet_wrap(~scenario, ncol = 1) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
  #ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
  #top
  
  diffbott <-  ggplot(df_diff %>% filter(dv == dvs[7]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.00047, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    #scale_fill_viridis(option="turbo") +
    facet_grid(~scenario) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) +   scale_fill_gradient2( name = "cfs_diff") + labs(x = NULL)
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[7], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "ridges_inflow.jpg", dpi = 300, width = 22, height = 13, units = "in") 

}

  #############################################
  #### K2  #####
  #############################################
  
  
  
  {
    ridgelinethickness <- 0.00000001
    top <- ggplot(df %>% filter(dv == dvs[2]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
      geom_ridgeline_gradient(scale = 0.00016, size = ridgelinethickness) + 
      #scale_fill_gradient(colors=turbo())  +
      scale_fill_viridis(option="turbo", name = "cfs     ") +
      facet_wrap(~scenario, ncol = 1) +
      scale_x_continuous(expand = c(0.02,0.02),
                         breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                         ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                         sec.axis = dup_axis(name = NULL)) +
      
      scale_y_continuous(expand = c(0,0),
                         breaks = seq(from = 2003, to = 2017, by = 1),
                         labels = sjrwytype$wy_wt,
                         sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
    #ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
    #top
    
    diffbott <-  ggplot(df_diff %>% filter(dv == dvs[2]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
      geom_ridgeline_gradient(scale = 0.0002, min_height = -3500, size = ridgelinethickness) + 
      #scale_fill_gradient(colors=turbo())  +
      #scale_fill_viridis(option="turbo") +
      facet_grid(~scenario) +
      scale_x_continuous(expand = c(0.02,0.02),
                         breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                         ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                         sec.axis = dup_axis(name = NULL)) +
      
      scale_y_continuous(expand = c(0,0),
                         breaks = seq(from = 2003, to = 2017, by = 1),
                         labels = sjrwytype$wy_wt,
                         sec.axis = dup_axis(name = NULL)) +   scale_fill_gradient2( name = "cfs_diff") + labs(x = NULL)
    
    p <- top / diffbott + plot_annotation(title = paste0(dvs[2], ' - 15 min data')) + 
      plot_layout(ncol = 1, heights = c(2,1))
    
    ggsave( "ridges_k2.jpg", dpi = 300, width = 22, height = 13, units = "in") 
    
  }
  

#############################################
#### Outage  #####
#############################################



{
  ridgelinethickness <- 0.00000001
  top <- ggplot(df %>% filter(dv == dvs[4]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0014, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    scale_fill_viridis(option="turbo", name = "cfs     ") +
    facet_wrap(~scenario, ncol = 1) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
  #ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
  #top
  
  diffbott <-  ggplot(df_diff %>% filter(dv == dvs[4]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0014, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    #scale_fill_viridis(option="turbo") +
    facet_grid(~scenario) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) +   scale_fill_gradient2( name = "cfs_diff") + labs(x = NULL)
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[4], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "ridges_outage.jpg", dpi = 300, width = 22, height = 13, units = "in") 
  
}



#############################################
#### Temp pulse  #####
#############################################



{
  ridgelinethickness <- 0.00000001
  top <- ggplot(df %>% filter(dv == dvs[10]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0045, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    scale_fill_viridis(option="turbo", name = "cfs     ") +
    facet_wrap(~scenario, ncol = 1) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
  #ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
  #top
  
  diffbott <-  ggplot(df_diff %>% filter(dv == dvs[10]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0045, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    #scale_fill_viridis(option="turbo") +
    facet_grid(~scenario) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) +   scale_fill_gradient2( name = "cfs_diff") + labs(x = NULL)
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[10], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "ridges_tempflows.jpg", dpi = 300, width = 22, height = 13, units = "in") 
  
}


#############################################
#### Spill Protocol  #####
#############################################



{
  ridgelinethickness <- 0.00000001
  top <- ggplot(df %>% filter(dv == dvs[5]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0045, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    scale_fill_viridis(option="turbo", name = "cfs     ") +
    facet_wrap(~scenario, ncol = 1) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
  #ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
  #top
  
  diffbott <-  ggplot(df_diff %>% filter(dv == dvs[5]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0045, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    #scale_fill_viridis(option="turbo") +
    facet_grid(~scenario) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) +   scale_fill_gradient2( name = "cfs_diff") + labs(x = NULL)
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[5], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "ridges_spillprtcl.jpg", dpi = 300, width = 22, height = 13, units = "in") 
  
}


#############################################
#### Whitewater raft pulse   #####
#############################################



{
  ridgelinethickness <- 0.00000001
  top <- ggplot(df %>% filter(dv == dvs[6]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0030, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    scale_fill_viridis(option="turbo", name = "cfs     ") +
    facet_wrap(~scenario, ncol = 1) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
  #ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
  #top
  
  diffbott <-  ggplot(df_diff %>% filter(dv == dvs[6]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0030, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    #scale_fill_viridis(option="turbo") +
    facet_grid(~scenario) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) +   scale_fill_gradient2( name = "cfs_diff") + labs(x = NULL)
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[6], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "ridges_whtwtrplse.jpg", dpi = 300, width = 22, height = 13, units = "in") 
  
}


#############################################
#### Storage  #####
#############################################



{
  ridgelinethickness <- 0.00000001
  top <- ggplot(df %>% filter(dv == dvs[9]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0003, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    scale_fill_viridis(option="turbo", name = "cfs     ") +
    facet_wrap(~scenario, ncol = 1) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) + labs(x = NULL)
  #ggsave( "top.jpg", dpi = 300, width = 13, height = 16, units = "in") 
  #top
  
  diffbott <-  ggplot(df_diff %>% filter(dv == dvs[9]), aes(dowy_hr, wy, group = as.factor(wy), height = value, fill = value )) + 
    geom_ridgeline_gradient(scale = 0.0012, min_height = -3500, size = ridgelinethickness) + 
    #scale_fill_gradient(colors=turbo())  +
    #scale_fill_viridis(option="turbo") +
    facet_grid(~scenario) +
    scale_x_continuous(expand = c(0.02,0.02),
                       breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                       ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0,0),
                       breaks = seq(from = 2003, to = 2017, by = 1),
                       labels = sjrwytype$wy_wt,
                       sec.axis = dup_axis(name = NULL)) +   scale_fill_gradient2( name = "cfs_diff") + labs(x = NULL)
  
  p <- top / diffbott + plot_annotation(title = paste0(dvs[9], ' - 15 min data')) + 
    plot_layout(ncol = 1, heights = c(2,1))
  
  ggsave( "ridges_storage.jpg", dpi = 300, width = 22, height = 13, units = "in") 
  
}

