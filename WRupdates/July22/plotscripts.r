ridgelinethickness <- 0.00000001
p <- ggplot(df, aes(dowy, wy, group = as.factor(wy), height = kcfsd, fill = mafwysum )) + 
  geom_ridgeline_gradient(scale = .03, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
  #scale_fill_gradient(colors=turbo())  +
  # scale_fill_viridis(option="turbo", name = "cfsd     ", direction = -1) +
  scale_fill_viridis( name = "maf     ", direction = 1) +
  # facet_wrap(~scenario, ncol = 1) +
  scale_x_continuous(expand = c(0.02,0.02),
                     breaks = c(1,32,62,93,124,152,183,213,244,274,305,336, 365) 
                     ,labels = c("O", "N", "D", "J", "F", "M", "A","M","J","J","A", "S", "O"),
                     sec.axis = dup_axis(name = NULL)) +
  
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(from = 1945, to = 2022, by = 5),
                     #labels = sjrwytype$wy_wt,
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL) + facet_grid(~name)
ggsave("Tues313pm.pdf", width = 23, height  = 13, units = "in")
