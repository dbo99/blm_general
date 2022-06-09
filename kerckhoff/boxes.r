# df <- df %>% mutate(scen_dv = paste0(scenario,"_", dv))
# df_diff <- df_diff %>% mutate(scen_dv = paste0(scenario,"_", dv))

#p <- ggplot(df_diff, aes(x = dv, y = value, group = dv)) + geom_boxplot() + facet_wrap(~wmt, ncol = 12)
#ggsave( "diffboxes_all2.jpg", dpi = 300, width = 40, height = 12, units = "in") 
#
#p <- ggplot(df, aes(x = dv, y = value, group = scen_dv, fill = scenario)) + geom_boxplot(position = "dodge") + facet_wrap(~wmt, ncol = 12)
#ggsave( "regboxes.jpg", dpi = 300, width = 40, height = 12, units = "in") 
#
#mons <- unique(df$wmt)
#lengmons <- seq(from = 1, to = length(mons), by = 1)

######################################
########### monthly boxes
######################################

p_box_justmonths <- function(df) {
 ggplot(df, aes(x = dv, y = value, group = scen_dv, fill = scenario)) + geom_boxplot(position = "dodge") + facet_wrap(~wmt, ncol = 1) +
         coord_flip()
}
  mons <- unique(df$wmt)
  lengmons <- seq(from = 1, to = length(mons), by = 1)


for(i in 1:length(wm$wm)) {                              # ggplot within for-loop
  p_box_justmonths(df %>% filter(wmt == mons[i])) +
    ggtitle(paste0(mons[i]))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                "_boxes.jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}

######################################
########### monthly & wyt boxes
######################################

p_box_monwyt <- function(df) {
  ggplot(df, aes(x = dv, y = value, group = scen_dv, fill = scenario)) + geom_boxplot(position = "dodge") + facet_wrap(~wmt_sjwytf, ncol = 1) +
    coord_flip()
}

mons <- unique(df$wmt_sjwytf)
lengmons <- seq(from = 1, to = length(mons), by = 1)


for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_box_monwyt(df %>% filter(wmt_sjwytf == mons[i])) +
  ggtitle(paste0(mons[i]))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                "_tukeyboxplots.jpg"), dpi = 300, width = 17, height = 11, units = "in") 
}

######################################
########### monthly & wyt boxes - diff
######################################

p_box_monwyt_diff <- function(df) {
  ggplot(df, aes(x = dv, y = value, group = scen_dv, fill = scenario)) + geom_boxplot(position = "dodge") + facet_wrap(~wmt_sjwytf, ncol = 1) +
    coord_flip()
}

mons <- unique(df_diff$wmt_sjwytf)
lengmons <- seq(from = 1, to = length(mons), by = 1)


for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_box_monwyt(df_diff %>% filter(wmt_sjwytf == mons[i])) +
    ggtitle(paste0(mons[i]), "(scenario difference)")
  ggsave(paste0(lengmons[i], " - ", mons[i],
                "_tukeyboxplots_diff.jpg"), dpi = 300, width = 17, height = 11, units = "in") 
}



######################################
########### every month boxes
######################################

p_box_everymon <- function(df) {
  ggplot(df, aes(x = dv, y = value, group = scen_dv, fill = scenario)) + geom_boxplot(position = "dodge") + facet_wrap(~wy_sjwytf, ncol = 1) +
    coord_flip()
}

mons <- unique(df$mon_yr_wy)
lengmons <- seq(from = 1, to = length(mons), by = 1)


for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_box_everymon(df %>% filter(mon_yr_wy == mons[i])) +
    ggtitle(paste0(mons[i]))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                "_tukeyboxplots.jpg"), dpi = 300, width = 17, height = 11, units = "in") 
}

#########################################################################################
#############################################################################################
###########################################################################
############ By variable
#########################################################################################
#############################################################################################
###########################################################################

######################
### just var ### 10 plots
#############################

df$wmt <- factor(df$wmt, levels = c("Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct"))
                                

p_box_justvars <- function(df) {
  ggplot(df, aes(x = wmt, y = value, group = dv_month_scen, fill = scenario)) + geom_boxplot(position = "dodge") + facet_wrap(~dv, ncol = 1) +
    coord_flip()
}
mons <- unique(df$dv)
lengmons <- seq(from = 1, to = length(mons), by = 1)


for(i in 1:length(wm$wm)) {                              # ggplot within for-loop
  p_box_justvars(df %>% filter(dv == mons[i])) +
    ggtitle(paste0(mons[i]))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                "_boxes.jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}


######################
### var & wytype ### (50 plots)
#############################
df$wmt <- factor(df$wmt, levels = c("Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct"))

p_box_varswyt <- function(df) {
  ggplot(df, aes(x = wmt, y = value, group = dv_monthyr_scen, fill = scenario)) + geom_boxplot(position = "dodge") + facet_wrap(~dv, ncol = 1) +
    coord_flip()
}
mons <- unique(df$dv_sjwytf)
lengmons <- seq(from = 1, to = length(mons), by = 1)


for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_box_varswyt(df %>% filter(dv_sjwytf == mons[i])) +
    ggtitle(paste0(mons[i]))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                "_boxes.jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}


######################
### var & wytype ### (50 plots)
#############################
df$wmt <- factor(df$wmt, levels = c("Sep", "Aug", "Jul", "Jun", "May", "Apr", "Mar", "Feb", "Jan", "Dec", "Nov", "Oct"))

p_box_varseverymonwyt <- function(df) {
  ggplot(df, aes(x = wmt, y = value, group = dv_eachmonthyr_scen, fill = scenario)) + geom_boxplot(position = "dodge") + facet_wrap(~dv, ncol = 1) +
    coord_flip()
}
mons <- unique(df$dv_wy_sjwytf)
lengmons <- seq(from = 1, to = length(mons), by = 1)


for(i in 1:length(mons)) {                              # ggplot within for-loop
  p_box_varseverymonwyt(df %>% filter(dv_wy_sjwytf == mons[i])) +
    ggtitle(paste0( mons[i]))
  ggsave(paste0(lengmons[i], " - ", mons[i],
                "_boxes.jpg"), dpi = 300, width = 22, height = 13, units = "in") 
}

