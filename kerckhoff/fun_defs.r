#read_plus <- function(flnm) {
#  read_csv(flnm)[-1,] %>%
#    mutate(filename = flnm)}

water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}



################################################
################################################
#create_df_diff <-function(df){
#  baseline_df <- df %>% filter(scenario == "baseline") %>% mutate(id = row_number())
#  
#df_diff <- df %>% select(scenario,  value) %>% group_by(scenario) %>% mutate(id = row_number()) %>% 
#  left_join(baseline_df, by = "id", suffix = c("_scenario", "_bl")) %>%  ungroup() %>%
#  mutate(value = value_scenario - value_bl, 
#         scenario = paste0(scenario_scenario," - bl")) %>%
#         select(-scenario_scenario, -scenario_bl) %>%
#         filter(scenario != "baseline - bl")
#
#
#wyt <- read_csv("sjr_wytype.csv") #%>% mutate(tstep = mdy(tstep)) #makes sure date reads in as date
#
#sjwyt_txt <- data.frame("sjwyt" = c(1,2,3,4,5), "sjwytt"=c("wt", "an", "bn", "dr", "cr"))
#
#df_diff  <- df_diff %>% inner_join(wyt) %>%  inner_join(sjwyt_txt) %>%
#  mutate( sjwyt_sjwytt = paste0(sjwyt, "_", sjwytt)) %>%
#  mutate( sjwyt = as.integer(sjwyt)) 
#
#}
#
#df_diff <- create_df_diff(df)



#p_mon_excd_value <- function(df) {
#  df %>% group_by(scenario, dv) %>% arrange(dv, desc(value)) %>% mutate(value_dv_rank = row_number(),
#        excdxaxis = value_dv_rank/(n()+1)) %>% ggplot(aes(x = excdxaxis, y = value, color = scenario,
#        linetype = dv)) + geom_point() + labs(x = "probability of exceedance", y = "value -- monthly")+theme_gray() +
#    guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
#    #scale_color_manual(values = df_cols) +
#    facet_grid(~scenario) +
#    ggtitle("984 months (value)") }

#p_mon_excd_value(df%>% filter(dv == "k2"))


