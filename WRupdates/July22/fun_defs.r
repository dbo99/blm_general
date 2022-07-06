#read_plus <- function(flnm) {
#  read_csv(flnm)[-1,] %>%
#    mutate(filename = flnm)}

water_year <- function(date) {
  ifelse(month(date) < 10, year(date), year(date)+1)}

water_month <-function(date) {
  ifelse(month(date) < 10, month(date)+3, month(date)-9)}

water_week <-function(date) {
  ifelse(strftime(date, format = "%V") >39, strftime(date, format = "%V") -39, trftime(date, format = "%V") + 13  )}

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}

ts_plot <- function(df, start_date, end_date) {
ggplot(df %>% filter( date >= start_date, date <= end_date), aes(x = date_time,
      y = value, color = scenario, linetype=scenario)) + geom_line()}


#rankplottest <- function(df) {
#  df %>%  
#    ggplot(aes(x = reorder_within(sjwyt, -scenariodiff, wy), y = scenariodiff, fill = wm, 
#              # label = round(mnanntaf_perav, 0)
#               )) + 
#    geom_bar(position = "dodge",stat = "position_dodge") + 
#    theme_gray()   + guides(colour = guide_legend(override.aes = list(size=2))) + theme(plot.margin=grid::unit(c(8,8,8,8), "mm")) +
#    ylab("taf") + 
#    scale_x_reordered() +
#   facet_wrap(~sjwyt, nrow = 1, scales = "free_x") +
#    theme(axis.title.x=element_blank(),axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
#    #theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#    scale_y_continuous(sec.axis = dup_axis(name = NULL) )+
#   # scale_fill_manual(values = df_cols)+
#    ggtitle("fuck yeah")
#}
              #    C        D       BN         AN             W
#labelcols <- c("coral4", "red", "orange", "light blue", "dark blue")

