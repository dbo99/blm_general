
#rm(list = ls())

rstudioapi::getActiveDocumentContext
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(MCMCglmm)
###########  wet (15 min) ###########

{
{
d1 <- rtnorm(n = 24*4, mean = 3250, sd = 100, lower = 3000, upper = 3500 )
d2 <- rtnorm(n = 24*4, mean = 2750, sd = 100, lower = 2500, upper = 3000 )
d3 <- rtnorm(n = 24*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
d4 <- rtnorm(n = 24*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
d5 <- rtnorm(n = 24*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
d6 <- rtnorm(n = 24*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
d7 <- rtnorm(n = 24*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
d8 <- rtnorm(n = 24*4, mean = 1800, sd = 100, lower = 1600, upper = 2000 )
d9 <- rtnorm(n = 24*4, mean = 1800, sd = 100, lower = 1600, upper = 2000 )
d10 <- rtnorm(n = 24*4, mean = 1450, sd = 100, lower = 1300, upper = 1600 )
d11 <- rtnorm(n = 24*4, mean = 1450, sd = 100, lower = 1300, upper = 1600 )
d12 <- rtnorm(n = 24*4, mean = 1150, sd = 100, lower = 1000, upper = 1300 )
d13 <- rtnorm(n = 24*4, mean = 1150, sd = 100, lower = 1000, upper = 1300 )
d14 <- rtnorm(n = 24*4, mean = 875, sd = 100, lower = 750, upper = 1000 )
d15 <- rtnorm(n = 24*4, mean = 875, sd = 100, lower = 750, upper = 1000 )
d16 <- rtnorm(n = 24*4, mean = 625, sd = 100, lower = 500, upper = 750 )
d17 <- rtnorm(n = 24*4, mean = 625, sd = 100, lower = 500, upper = 750 )
d18 <- rtnorm(n = 24*4, mean = 375, sd = 100, lower = 250, upper = 500 )
d19 <- rtnorm(n = 24*4, mean = 375, sd = 100, lower = 250, upper = 500 )
d20 <- rtnorm(n = 24*4, mean = 175, sd = 100, lower = 100, upper = 250 )
d21 <- rtnorm(n = 24*4, mean = 62.5, sd = 100, lower = 25, upper = 100 )}

cfs <- c(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21)
rm(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21)
x  <- seq(from=1,to=length(cfs),by=1)
df_d <- data.frame(x, cfs) %>% mutate(cond = "water year type d\ngeneral spill rampdown", cond2 = "d")
ggplot(df_d, aes(x, cfs)) + geom_line()



############ above or below normal  (15 min) #################
{
d1 <- rtnorm(n = 24*4, mean = 3250, sd = 100, lower = 3000, upper = 3500 )
d2 <- rtnorm(n = 24*4, mean = 2750, sd = 100, lower = 2500, upper = 3000 )
d3 <- rtnorm(n = 24*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
d4 <- rtnorm(n = 24*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
d5 <- rtnorm(n = 24*4, mean = 1800, sd = 100, lower = 1600, upper = 2000 )
d6 <- rtnorm(n = 18*4, mean = 1400, sd = 100, lower = 1200, upper = 1600 )   # starts at 6pm (not full day)
d7a <- rtnorm(n = 8*4, mean = 1000, sd = 100, lower = 800, upper = 1200 )    # starts at 2am
d7b <- rtnorm(n = 10*4, mean = 700, sd = 100, lower = 600, upper = 800 )     # starts at noon
d7c <- rtnorm(n = 5*4, mean = 500, sd = 100, lower = 400, upper = 600 )      # starts at 5pm
d7d <- rtnorm(n = 5*4, mean = 300, sd = 100, lower = 200, upper = 400 )      # starts at 10pm
d7ed8 <- rtnorm(n = 5*4, mean = 112.5, sd = 100, lower = 25, upper = 200 )}  # starts at 3am

cfs <- c(d1,d2,d3,d4,d5,d6,d7a, d7b, d7c,d7d, d7ed8)
rm(d1,d2,d3,d4,d5,d6,d7a, d7b, d7c,d7d, d7ed8)
x  <- seq(from=1,to=length(cfs),by=1)
df_cb <- data.frame(x, cfs) %>% mutate(cond = "water year type c/b\ngeneral spill rampdown", cond2 = "cb")
ggplot(df_cb, aes(x, cfs)) + geom_line()


##############  dry or critical (15 min) ####################

{
d1 <- rtnorm(n = 24*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
d2 <- rtnorm(n = 24*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
d3 <- rtnorm(n = 18*4, mean = 1800, sd = 100, lower = 1600, upper = 2000 ) # starts at 6pm (not full day)
d4a <- rtnorm(n = 8*4, mean = 1400, sd = 100, lower = 1200, upper = 1600 )
d4b <- rtnorm(n = 10*4, mean = 1000, sd = 100, lower = 800, upper = 1200 )
d4c <- rtnorm(n = 5*4, mean = 700, sd = 100, lower = 600, upper = 800 )
d4d <- rtnorm(n = 5*4, mean = 500, sd = 100, lower = 400, upper = 600 )
d5a <- rtnorm(n = 5*4, mean = 300, sd = 100, lower = 200, upper = 400 )
d5b <- rtnorm(n = 5*4, mean = 112.5, sd = 100, lower = 25, upper = 200 )}

cfs <- c(d1, d2, d3,d4a,d4b,d4c,d4d,d5a, d5b)
rm(d1, d2, d3,d4a,d4b,d4c,d4d,d5a, d5b)
x  <- seq(from=1,to=length(cfs),by=1)
df_ab <- data.frame(x, cfs) %>% mutate(cond = "water year type a/b\ngeneral spill rampdown", cond2 = "ab")
ggplot(df_ab, aes(x, cfs)) + geom_line()


########### Non-Spill Year Boating Flow Schedule (whitewater release) (15 min) ###################### 

{

  s1 <- rtnorm(n = 9*4, mean = 2250, sd = 100, lower = 2000, upper = 2500 )
  s2 <- rtnorm(n = 24*4, mean = 1800, sd = 100, lower = 1600, upper = 2000 )
  s3 <- rtnorm(n = 10*4, mean = 1400, sd = 100, lower = 1200, upper = 1600 )
  s4 <- rtnorm(n = 5*4, mean = 1000, sd = 100, lower = 800, upper = 1200 )
  s5 <- rtnorm(n = 5*4, mean = 700, sd = 100, lower = 600, upper = 800 )
  s6 <- rtnorm(n = 5*4, mean = 500, sd = 100, lower = 400, upper = 600 )
  s7 <- rtnorm(n = 5*4, mean = 300, sd = 100, lower = 200, upper = 400 )}

cfs <- c(s1,s2,s3,s4,s5,s6,s7)
rm(s1,s2,s3,s4,s5,s6,s7)
x  <- seq(from=1,to=length(cfs),by=1)
df_ww <- data.frame(x, cfs) %>% mutate(cond = "5/15 - 6/15 non spill yr\n boating rel.", cond2 = "ww")
ggplot(df_ww, aes(x, cfs)) + geom_line()



########### stranding protection (15 min) ###################### 

{
  
  s1 <- rtnorm(n = 10*4, mean = 1400, sd = 150, lower = 1200, upper = 1600 )
  s2 <- rtnorm(n = 10*4, mean = 1000, sd = 150, lower = 800, upper = 1200 )
  s3 <- rtnorm(n = 5*4, mean = 700, sd = 150, lower = 600, upper = 800 )
  s4 <- rtnorm(n = 5*4, mean = 500, sd = 150, lower = 400, upper = 600 )
  s5 <- rtnorm(n = 5*4, mean = 300, sd = 150, lower = 200, upper = 400 )
  s6 <- rtnorm(n = 2*4, mean = 112.5, sd = 150, lower = 25, upper = 200 )}
  
cfs <- c(s1,s2,s3,s4,s5,s6)
rm(s1,s2,s3,s4,s5,s6)
x  <- seq(from=1,to=length(cfs),by=1)
df_sp <- data.frame(x, cfs) %>% mutate(cond = "5/1 - 8/31 stranding protection\nspill downramp", cond2 = "sp")
ggplot(df_sp, aes(x, cfs)) + geom_line()}

################## rbind #############

df <- rbind(df_ab, df_cb, df_d, df_sp, df_ww) %>% mutate(day = x/(24*4), af = cfs*(0.082644/4))
df_afsum <- df %>% group_by(cond) %>% summarize(af_cond = sum(af), prcnt_avg_inflow = af_cond/1632116*100)
df <- inner_join(df, df_afsum)




blmconds_15min <- df
rm(df, df_ab, df_cb, df_d, df_sp, df_ww, cfs, x, df_afsum) 
p1 <- ggplot(blmconds_15min, aes(day, cfs)) + geom_line() + facet_wrap(~cond) + ggtitle("DOI-proposed recessions")
p1
ggsave("recession_conditions.jpeg", width = 16, height = 9, unit = "in")

p2 <- ggplot(blmconds_15min, aes(day, cfs, fill = af_cond/1000)) + geom_area() + facet_wrap(~cond) + ggtitle("DOI-proposed recessions") +scale_fill_viridis(name = "ungenerated kaf")
p2
ggsave("recession_conditions_withAF.jpeg", width = 16, height = 9, unit = "in")

p3 <- ggplot(blmconds_15min, aes(day, cfs, fill = prcnt_avg_inflow)) + geom_area() + facet_wrap(~cond) + ggtitle("DOI-proposed recessions") +scale_fill_viridis(name = "% WY inflow")
p3
ggsave("recession_conditions_withAF_prcntinflow.jpeg", width = 16, height = 9, unit = "in")
