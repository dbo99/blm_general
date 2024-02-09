



rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("fun_defs.r")

#11446500 #fair oaks

siteNo <- "11446500"
pCode <- "00060" 
start.date <- "1987-10-01"
end.date <- "2023-09-30"


amrcn_fo <- readNWISuv(siteNumbers = siteNo,
                       parameterCd = pCode,
                       startDate = start.date,
                       endDate = end.date)

amrcn_fo <- renameNWISColumns(amrcn_fo)
names(amrcn_fo)

df <- amrcn_fo %>% mutate(wy= water_year(dateTime))
head(df)

df_annpeak <- df %>% filter(wy != 2024) %>% group_by(wy) %>% summarize(peak = max(Flow_Inst)) #%>% arrange(desc(peak))
df_annpeak
#write_csv(df_annpeak, "amrcn_fairoaks_wy_instantaneous_peakflow.txt")
ggplot(df_annpeak, aes(wy, peak/1000)) + geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(1987, 1997, 2006, 2011, 2017, 2023)) + 
  scale_y_continuous(breaks = c(10, 30, 50, 85.4, 117 )) + 
  labs(x = "water year", y = "peak instantaneous flow rate (kcfs)") + gghighlight(wy == 2017) +
  ggtitle("American River at Fair Oaks, 11446500" )




# stage
start.date <- "2007-10-01"
end.date <- "2023-09-30"
pCode <- "00065" #00065 is stage, 00060 is flow
amrcn_fo <- readNWISuv(siteNumbers = siteNo,
                       parameterCd = pCode,
                       startDate = start.date,
                       endDate = end.date)

amrcn_fo <- renameNWISColumns(amrcn_fo)
names(amrcn_fo)

df <- amrcn_fo %>% mutate(wy= water_year(dateTime))
head(df)

df_annpeak <- df %>% group_by(wy) %>% summarize(peak = max(GH_Inst)) %>% arrange(desc(peak))
df_annpeak
write_csv(df_annpeak, "amrcn_fairoaks_wy_instantaneous_peakstage_ft.txt")
