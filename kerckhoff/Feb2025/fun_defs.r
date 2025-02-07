water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}

water_week <-function(date) {
  ifelse(strftime(date, format = "%V") >39, strftime(date, format = "%V") -39, trftime(date, format = "%V") + 13  )}

water_week2 <-function(date) {
  ifelse(week(date) >39, week(date) -39, week(date) + 13  )}
