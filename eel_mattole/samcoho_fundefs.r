water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}


create_dly_quants <- function(df, grouping_var, summary_var){
  grouping_var <- enquo(grouping_var)
  summary_var  <- enquo(summary_var)
  summary_nm   <- quo_name(summary_var)
  summary_nm_mean <- paste0("mean")
  summary_nm_median <- paste0("median")
  summary_nm_min <- paste0("min")
  summary_nm_max <- paste0("max")
  summary_nm_05prcnt <- paste0("5%")
  summary_nm_10prcnt <- paste0("10%")
  summary_nm_25prcnt <- paste0("25%")
  summary_nm_50prcnt <- paste0("50%")
  summary_nm_75prcnt <- paste0("75%")
  summary_nm_90prcnt <- paste0("90%")
  summary_nm_95prcnt <- paste0("95%")

  df %>%
    group_by(!!grouping_var) %>% 
    summarize(
      !!summary_nm_mean    := mean(!!summary_var),
      !!summary_nm_median  := median(!!summary_var),
      !!summary_nm_min     := min(!!summary_var),
      !!summary_nm_max     := max(!!summary_var),
      !!summary_nm_05prcnt := quantile(!!summary_var, probs = 0.05),
      !!summary_nm_10prcnt := quantile(!!summary_var, probs = 0.10),
      !!summary_nm_25prcnt := quantile(!!summary_var, probs = 0.25),
      !!summary_nm_50prcnt := quantile(!!summary_var, probs = 0.50),
      !!summary_nm_75prcnt := quantile(!!summary_var, probs = 0.75),
      !!summary_nm_90prcnt := quantile(!!summary_var, probs = 0.90),
      !!summary_nm_95prcnt := quantile(!!summary_var, probs = 0.95)      
    )
}



