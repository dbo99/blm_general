library(tidyverse)
library(lubridate)
library(ggridges)
library(viridis)
#library(zoo)
#library(plotly)
library(gghighlight)
library(patchwork)


{
rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("fun_defs.r")
dowy <- read_csv("daily_dowy.csv") %>% mutate(date = ymd(date))
head(dowy)



files <- c("HLEC1_daily.csv", "CEGC1_daily.csv", "EXQC1_daily.csv", "FOLC1_daily.csv", "FRAC1_daily.csv",
           "ISAC1_daily.csv", "NDPC1_daily.csv", "ORDC1_daily.csv", "PFTC1_daily.csv", "SHDC1_daily.csv", 
           "CMPC1_daily.csv", "MHBC1_daily.csv", "TMDC1_daily.csv", "SCSC1_daily.csv", "NMSC1_daily.csv")


hlec1 <- readKylesdailyFNFcsv(files[1]) 
cegc1 <- readKylesdailyFNFcsv(files[2])
exqc1 <- readKylesdailyFNFcsv(files[3])
folc1 <- readKylesdailyFNFcsv(files[4])
frac1 <- readKylesdailyFNFcsv(files[5])
isac1 <- readKylesdailyFNFcsv(files[6])
ndpc1 <- readKylesdailyFNFcsv(files[7])
ordc1 <- readKylesdailyFNFcsv(files[8])
pftc1 <- readKylesdailyFNFcsv(files[9])
shdc1 <- readKylesdailyFNFcsv(files[10])
cmpc1 <- readKylesdailyFNFcsv(files[11])
mhbc1 <- readKylesdailyFNFcsv(files[12])
tmdc1 <- readKylesdailyFNFcsv(files[13])
scsc1 <- readKylesdailyFNFcsv(files[14])
nmsc1 <- readKylesdailyFNFcsv(files[15])

head(shdc1)
df <- rbind(hlec1, cegc1, exqc1, folc1, frac1,
            isac1, ndpc1, ordc1, pftc1, shdc1,
            cmpc1, mhbc1, tmdc1, scsc1, nmsc1) %>% mutate(wy = water_year(date))

df <- inner_join(df, dowy)
head(df)
}

############ convert flow to volume and aggregate into water year sum


res <- c("SHDC1", "CEGC1", "ORDC1", "FOLC1", "NDPC1", "EXQC1", "FRAC1", "PFTC1",
         "ISAC1", "HLEC1", "CMPC1", "MHBC1", "NMSC1", "TMDC1", "SCSC1")
name <- c("Shasta", "Trinity", "Oroville", "Folsom", "Don Pedro", "McClure",
          "Millerton", "Pine Flat", "Isabella", "Englebright", "Pardee", "Mich. Bar",
          "New Melones", "Terminus", "Success")
river <- c("Sac/McCld/Pit", "Trinity", "Feather", "American", "Tuolumne", "Merced", "San Joaquin", 
           "Kings", "Kern", "Yuba", "Mokelumne", "Cosumnes", "Stanislaus", "Kaweah", "Tule")
resname <- data.frame(res, name, river)
#for water year to water year comparison, use forecast (few weeks left in water year, high forecast skill) mafwysum = wy22fcast for wy22
mafwysum <- c(2.890, .486, 2.840, 1.870, 1.06, .467, 1.03, .747,
               .196, 1.43, .48, .218, .669, .148, .033)
wy <- rep(2022, length(res))

df_wysum <- df %>% mutate(wy = water_year(date), 
                          maf = kcfsd*0.00198347107) %>% group_by(res,wy) %>%
                  summarize(mafwysum = sum(maf, na.rm=TRUE)) 
head(df_wysum)
tail(df_wysum)
#remove wy 2022 obs and replace with forecasts
#remove
df_wysum <- df_wysum %>% filter(wy != 2022)
#2022 fcasts to append
wy22fcasts <- data.frame(res, wy, mafwysum)
# append
df_wysum <- rbind(df_wysum, wy22fcasts)
head(df_wysum)
tail(df_wysum) 

df_wysum <- inner_join(df_wysum, resname)  %>% mutate(rivres = paste0(river, " - ", name))
head(df_wysum)
tail(df_wysum) 
  unique(df_wysum$rivres)
  unique(df_wysum$res)
#################################

df_wysum_w <- df_wysum  %>% ungroup() %>% transmute(rivres, wy, mafwysum) %>% pivot_wider(values_from = mafwysum, names_from = rivres ) %>% arrange(desc(wy))
head(df_wysum_w)
df_wysum_w$rivres <- factor(df_wysum_w$rivres, 
                          levels = c("Sac/McCld/Pit - Shasta" , "Trinity - Trinity", "Feather - Oroville", "Yuba - Englebright","American - Folsom",
                                     "Cosumnes - Mich. Bar", "Mokelumne - Pardee", "Stanislaus - New Melones", "Tuolumne - Don Pedro", "Merced - McClure", 
                                     "San Joaquin - Millerton", "Kings - Pine Flat", "Kaweah - Terminus", "Tule - Success", "Kern - Isabella")) 
head(df_wysum_w)
write_csv(df_wysum_w, "df_wysum.csv")

#### rank
df_wysum_rank <- df_wysum %>% ungroup() %>% transmute(rivres, wy, mafwysum) %>% group_by(rivres) %>%
                   mutate(rank = rank(mafwysum)) %>% transmute(rivres, wy, rank) %>% arrange(desc(wy))
head(df_wysum_rank)
tail(df_wysum_rank)

df_wysum_rank_w <- df_wysum %>% ungroup() %>% transmute(rivres, wy, mafwysum) %>% group_by(rivres) %>%
                       mutate(rank = rank(mafwysum)) %>% transmute(rivres, wy, rank) %>% 
                       pivot_wider(names_from = rivres, values_from = rank) %>% 
                       arrange(desc(wy))
head(df_wysum_rank_w)
tail(df_wysum_rank_w)

#add wy sum to every row for coloring whole ridge same annual value
df <- inner_join(df, df_wysum)
head(df)

df <- inner_join(df, resname)
head(df)
df$name <- factor(df$name, 
                  levels = c("Shasta" , "Trinity", "Oroville", "Englebright","Folsom",
                             "Mich. Bar", "Pardee", "New Melones", "Don Pedro", "McClure", 
                              "Millerton", "Pine Flat", "Terminus", "Success", "Isabella"))
unique(df$rivres)
df$rivres <- factor(df$rivres, 
                  levels = c("Sac/McCld/Pit - Shasta" , "Trinity - Trinity", "Feather - Oroville", "Yuba - Englebright","American - Folsom",
                             "Cosumnes - Mich. Bar", "Mokelumne - Pardee", "Stanislaus - New Melones", "Tuolumne - Don Pedro", "Merced - McClure", 
                             "San Joaquin - Millerton", "Kings - Pine Flat", "Kaweah - Terminus", "Tule - Success", "Kern - Isabella"))   

df_wysum$rivres <- factor(df_wysum$rivres, 
                    levels = c("Sac/McCld/Pit - Shasta" , "Trinity - Trinity", "Feather - Oroville", "Yuba - Englebright","American - Folsom",
                               "Cosumnes - Mich. Bar", "Mokelumne - Pardee", "Stanislaus - New Melones", "Tuolumne - Don Pedro", "Merced - McClure", 
                               "San Joaquin - Millerton", "Kings - Pine Flat", "Kaweah - Terminus", "Tule - Success", "Kern - Isabella")) 
#}


rm(list=setdiff(ls(), c("df", "df_wysum", "df_wysum_w", "dowy", "df_wysum_rank", "df_wysum_rank_w", "resname")))
source("fun_defs.r")
## data.frames combining all above inflow into one "central valley inflow"

## daily ##
df1980_15restotal <- df %>% filter (wy >= 1980) %>% mutate(cfsd = 1000*kcfsd, af = 1.98347*cfsd, kaf = af/1000, maf = kaf/1000) %>%
  group_by(date) %>% summarize(maf = sum(maf))
head(df1980_15restotal)
tail(df1980_15restotal)


df1980_15restotal <- inner_join(df1980_15restotal, dowy) %>% mutate(wy = water_year(date))
head(df1980_15restotal)
tail(df1980_15restotal)


## annual ##
df1980_15restotal_wy <- df %>% filter (wy >= 1980) %>% mutate(cfsd = 1000*kcfsd, af = 1.98347*cfsd, kaf = af/1000, maf = kaf/1000) %>%
  group_by(wy) %>% summarize(totalvol_maf = sum(maf)) 
 # summarize(mafwysum = sum(totalvol_maf))
head(df1980_15restotal_wy)
tail(df1980_15restotal_wy)

df1980_15restotal <- inner_join(df1980_15restotal, df1980_15restotal_wy)



####### read in excel sheet for multi-year accumulations ########
## not sure why i wanted rank
#df_3yrcumrank_w <- read_csv("forR_3wyrank.csv")
#head(df_3yrcumrank_w)
#df_3yrcumrank <- df_3yrcumrank_w %>% pivot_longer(!wys, names_to = "rank")
#head(df_3yrcumrank)

df_3yrvol_w <- read_csv("df_wysum_dbo_csv.csv") 
head(df_3yrvol_w)
tail(df_3yrvol_w)
df_3yrvol <- read_csv("df_wysum_dbo_csv.csv") %>% pivot_longer(!wys, names_to = "res") %>% 
              filter(res == "all") %>% transmute(wys, res, maf = value, endyear = str_sub(wys,-4,-1)) 
df_3yrvol <- df_3yrvol %>% mutate(endyear = as.integer(endyear)) %>% filter(endyear >= 1982)
                                                 
head(df_3yrvol)
tail(df_3yrvol)
ggplot(df_3yrvol, aes(endyear, maf)) + geom_point()

