#read_plus <- function(flnm) {
#  read_csv(flnm)[-1,] %>%
#    mutate(filename = flnm)}

water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}

water_week <-function(date) {
  ifelse(strftime(date, format = "%V") >39, strftime(date, format = "%V") -39, trftime(date, format = "%V") + 13  )}

#reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
#  new_x <- paste(x, within, sep = sep)
#  stats::reorder(new_x, by, FUN = fun)
#}
#
#scale_x_reordered <- function(..., sep = "___") {
#  reg <- paste0(sep, ".+$")
#  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
#}
#
#scale_y_reordered <- function(..., sep = "___") {
#  reg <- paste0(sep, ".+$")
#  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
#}
#
#ts_plot <- function(df, start_date, end_date) {
#ggplot(df %>% filter( date >= start_date, date <= end_date), aes(x = date_time,
#      y = value, color = scenario, linetype=scenario)) + geom_line()}
#
#readKylesdailyFNFcsv <- function(file) {
#  read_csv(file) %>% transmute(date = `Date (ending 12 UTC)`, kcfsd = `Flow (KCFS)`, res = paste0(file), res = substr(res,1,5))}


