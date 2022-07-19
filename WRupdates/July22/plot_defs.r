## ridge with all 



ridgelinethickness <- 0.00000001
p <- ggplot(df %>% filter(res == "SHDC1" | res == "PFTC1"), aes(dowy, wy, group = as.factor(wy), height = kcfsd, fill = mafwysum )) + 
  geom_ridgeline_gradient(scale = .05, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
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
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL) + facet_grid(~name) +
  ggtitle("Lake Shasta vs Pine Flat\nFNF Volume per water wear (forecast volume used for 2022)") +
   geom_vline(xintercept = 183, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 305, linetype = "dashed", color = "red")
p
ggsave("shasta&pine.jpg", width = 13.5, height  = 7.5, dpi = 300, units = "in")

############ for 15 basin total
head(df1980_15restotal)
p <- ggplot(df1980_15restotal, aes(dowy, wy, group = as.factor(wy), height = maf, fill = totalvol_maf )) + 
  geom_ridgeline_gradient(scale = 5.5, min_height = -3500, size = ridgelinethickness, alpha = 0.5) + 
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
                     sec.axis = dup_axis(name = NULL)) + labs(x = NULL) + #facet_grid(~name) +
  ggtitle("FNF per water year (forecast used for remainder of 2022) -- combined 15 basin total inflow volume")
p
ggsave("totaled15.jpg", width = 13.5, height  = 7.5, dpi = 300, units = "in")

#################### exceedance wy all facets #####################
head(df)
head(df_wysum)

## time series wy sum res facet

p_ts_wysum_resfacet <- function(df) {
  
  df   %>% mutate(xlabel = round(mafwysum, 1)) %>%
    ggplot(aes(x = wy, y = mafwysum)) +
    geom_point() + labs(x = "wy", y = "water year inflow volume (maf)")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    
    geom_smooth(method = "lm", se = FALSE) +
    
    scale_x_continuous(
                  
                       sec.axis = dup_axis(name = NULL)) +
    
    #scale_fill_viridis( name = "percentile", direction = 1) +
    facet_wrap(~rivres, scales = "free_y", nrow =5) + #theme(strip.text.y.left = element_text(angle = 0)) +
    #ggtitle(paste0(unique(df$wmt),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    
    
    scale_y_continuous(expand = c(0.1,0.1)) + gghighlight(wy == 2022, label_key = xlabel) }
p_ts_wysum_resfacet(df_wysum)
ggsave("wysum_ts2.jpg", width = 10, height  = 6.5, units = "in", dpi = 300)

## wysum exceedanc
p_excd_wysum_resfacet <- function(df) {
  
  df   %>% 
    group_by(res) %>% arrange( mafwysum) %>% 
    mutate(rank = row_number(), 
           excdxaxis = rank/(n()+1),
           xlabel = round(excdxaxis,2)) %>% ggplot(aes(x = excdxaxis, y = mafwysum)) +
    geom_point() + labs(x = "percentile", y = "water year inflow volume (maf)")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    
    #scale_fill_viridis( name = "percentile", direction = 1) +
    facet_wrap(~rivres, scales = "free_y", nrow =5) + #theme(strip.text.y.left = element_text(angle = 0)) +
    #ggtitle(paste0(unique(df$wmt),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    scale_x_continuous(
                       breaks = seq(from = 0.0, to = 1.0, by = .1),
                       labels = seq(from = 0.0, to = 1.0, by = .1),
                       sec.axis = dup_axis(name = NULL)) +
    
    scale_y_continuous(expand = c(0.005,0.005)) + gghighlight(wy == 2022, label_key = xlabel)}
p_excd_wysum_resfacet(df_wysum)
ggsave("resinflowexceed.jpg", width = 10, height = 6.5, units = "in")
p_wysumtstopexcdbott <- function(df) {
  p1 <- p_ts_wysum_resfacet(df)
  p2 <- p_excd_wysum_resfacet(df)
  p1 | p2
}
p_wysumtstopexcdbott(df_wysum)
ggsave("wysum_ts&excd.jpg", width = 23, height  = 13, units = "in", dpi = 300)

###################################################
### 15 basin totals, 1 yr 
#############################################
head(df1980_15restotal_wy)
#### scatter total sierra wy
p_ts_total15_with_trend <- function(df) {
 df %>% mutate(label = round(totalvol_maf,1)) %>% ggplot( aes(wy, totalvol_maf)) + 
 geom_line() + geom_point() +  #geom_smooth(method = "lm", se = FALSE)  +# gghighlight(wy == 2022) +
    labs(x = "water year", y = "water year inflow volume (maf)") +
    ggtitle("15 basin total, single water year volume")  +  gghighlight(wy == 2022, label_key = label) + 
    scale_y_continuous(expand = c(0.005,0.005),
                       breaks = c(10,20,30,40,50,60),
                       labels = c(10,20,30,40,50,60))}
p_ts_total15_with_trend(df1980_15restotal_wy)

p_excd_total15 <- function(df) {
  df %>% arrange( totalvol_maf) %>% 
    mutate(rank = row_number(), 
           excdxaxis = rank/(n()+1),
           xlabel = round(excdxaxis,2)) %>% ggplot(aes(x = excdxaxis, y = totalvol_maf)) +
    geom_point() + labs(x = "percentile", y = "water year inflow volume (maf)")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    
    #scale_fill_viridis( name = "percentile", direction = 1) +
    #facet_wrap(~rivres, scales = "free_y", nrow =5) + #theme(strip.text.y.left = element_text(angle = 0)) +
    #ggtitle(paste0(unique(df$wmt),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    scale_x_continuous(expand = c(0.005,0.005),
                       breaks = seq(from = 0.0, to = 1.0, by = .1),
                       labels = seq(from = 0.0, to = 1.0, by = .1)) +
    
    scale_y_continuous(expand = c(0.005,0.005),
                       breaks = c(10,20,30,40,50,60),
                       labels = c(10,20,30,40,50,60)) + 
    
    #scale_y_continuous(expand = c(0.005,0.005)) + 
    #scale_color_viridis() + #geom_point(data = df %>% filter(wy == 2022), aes(x = wy, y = mafwysum ), color = "red") + 
    gghighlight(wy == 2022, label_key = xlabel) +
    ggtitle("15 basin total, single water year volume")}
p_excd_total15(df1980_15restotal_wy)

p_15restotal_scattAndexcd <- function(df) {
  p1 <- p_ts_total15_with_trend(df)
  p2 <- p_excd_total15(df)
  p1 | p2
}
p_15restotal_scattAndexcd(df1980_15restotal_wy)
ggsave("15basins_1yr.jpg", width = 10, height  = 6, units = "in", dpi = 300)

###################################################
### 15 basin totals, 3 yrs 
#############################################
head(df_3yrvol) 
head(df_3yrvol_w)

head(df1980_15restotal_wy)
#### scatter total sierra wy
p_ts_total15_with_trend <- function(df) {
  df %>% mutate(label = round(maf,1)) %>% ggplot( aes(endyear, maf)) + 
    geom_line() + geom_point() +  #geom_smooth(method = "lm", se = FALSE)  +# gghighlight(wy == 2022) +
    labs(x = "end water year of three year period", y = "water year inflow volume (maf)") +
    ggtitle("15 basin total, three water year volume")  }#+ gghighlight(wy == 2022, label_key = label) }

p_ts_total15_with_trend(df_3yrvol)

p_excd_total15 <- function(df) {
  df %>% arrange( maf) %>% 
    mutate(rank = row_number(), 
           excdxaxis = rank/(n()+1),
           xlabel = round(excdxaxis,2)) %>% ggplot(aes(x = excdxaxis, y = maf)) +
    geom_point() + labs(x = "percentile", y = "water year inflow volume (maf)")+ #theme_gray() +
    #guides(colour = guide_legend(override.aes = list(size=2))) + 
    
    #scale_fill_viridis( name = "percentile", direction = 1) +
    #facet_wrap(~rivres, scales = "free_y", nrow =5) + #theme(strip.text.y.left = element_text(angle = 0)) +
    #ggtitle(paste0(unique(df$wmt),"'s - WYs ", unique(year((min(df$date_time)))),"-", unique(year((max(df$date_time)))), " (no '04)")) + 
    scale_x_continuous(expand = c(0.005,0.005),
                       breaks = seq(from = 0.0, to = 1.0, by = .1),
                       labels = seq(from = 0.0, to = 1.0, by = .1)) +

    #scale_y_continuous(expand = c(0.005,0.005)) + 
    #scale_color_viridis() + #geom_point(data = df %>% filter(wy == 2022), aes(x = wy, y = mafwysum ), color = "red") + 
   gghighlight(endyear == 2022, label_key = xlabel) +
    ggtitle("15 basin total, three water year volume")}
p_excd_total15(df_3yrvol)

p_15restotal_scattAndexcd <- function(df) {
  p1 <- p_ts_total15_with_trend(df)
  p2 <- p_excd_total15(df)
  p1 | p2
}
p_15restotal_scattAndexcd(df_3yrvol)
ggsave("15basins_3yr.jpg", width = 10, height  = 6, units = "in", dpi = 300)

