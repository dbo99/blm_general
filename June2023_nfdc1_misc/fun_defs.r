water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}


#create_hrly_quants <- function(df, hourcolumn, cfs)   {
#  
#  cfs <- rlang::sym(cfs)
#  
#  df  %>% group_by_at(vars(hourcolumn)) %>%
#    summarize(
#       `5%`=quantile(!!cfs, probs=0.05), 
#      `10%`=quantile(!!cfs, probs=0.10),
#      `25%`=quantile(!!cfs, probs=0.25),
#      `50%`=quantile(!!cfs, probs=0.5),
#      `75%`=quantile(!!cfs, probs=0.75),
#      `90%`=quantile(!!cfs, probs=0.9),
#      `95%`=quantile(!!cfs, probs=0.95),
#      `min`=min(!!cfs), `max`=max(!!cfs), mean=mean(!!cfs), median=median(!!cfs), 
#      n=n()) 
#}


