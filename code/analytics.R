library(installr)
library(dplyr)
library(ggplot2)
library(haven)
library(MASS)
library(foreign)
library(tseries)
library(forecast)
library(TTR)
library(readxl)
library(Hmisc)

###?ð迭?м? 
##?????? ???? ???????? 
#did female
sex_getjob_did_fe <- read_xlsx("/sex_getjob.xlsx", sheet=2)
sex_getjob_did_fe_ts <- ts(sex_getjob_did_fe)
plot.ts(sex_getjob_did_fe_ts)

#did male
sex_getjob_did_ma <- read_xlsx("/sex_getjob.xlsx", sheet=3)
sex_getjob_did_ma_ts <- ts(sex_getjob_did_ma)
plot.ts(sex_getjob_did_ma_ts)

#nopay female
sex_getjob_nopay_fe <- read_xlsx("/sex_getjob.xlsx", sheet=6)
sex_getjob_nopay_fe_ts <- ts(sex_getjob_nopay_fe)
plot.ts(sex_getjob_nopay_fe_ts)

#nopay male
sex_getjob_nopay_ma <- read_xlsx("/sex_getjob.xlsx", sheet=7)
sex_getjob_nopay_ma_ts <- ts(sex_getjob_nopay_ma)
plot.ts(sex_getjob_nopay_ma_ts)

##?????? ???? ????????
#mid did
ages_getjob_mid_did <- read_xlsx("/ages_getjob.xlsx", sheet=2)
ages_getjob_mid_did_ts <- ts(ages_getjob_mid_did)
plot.ts(ages_getjob_mid_did_ts)

#mid nopay 
ages_getjob_mid_nopay <- read_xlsx("/ages_getjob.xlsx", sheet=4)
ages_getjob_mid_nopay_ts <- ts(ages_getjob_mid_nopay)
plot.ts(ages_getjob_mid_nopay_ts)

#young did 
ages_getjob_young_did <- read_xlsx("/ages_getjob.xlsx", sheet=5)
ages_getjob_young_did_ts <- ts(ages_getjob_young_did)
plot.ts(ages_getjob_young_did_ts)

#young nopay
ages_getjob_young_nopay <- read_xlsx("/ages_getjob.xlsx", sheet=7)
ages_getjob_young_nopay_ts <- ts(ages_getjob_young_nopay)
plot.ts(ages_getjob_young_nopay_ts)

#old did 
ages_getjob_old_did <- read_xlsx("/ages_getjob.xlsx", sheet=8)
ages_getjob_old_did_ts <- ts(ages_getjob_old_did)
plot.ts(ages_getjob_old_did_ts)

#old nopay
ages_getjob_old_nopay <- read_xlsx("/ages_getjob.xlsx", sheet=9)
ages_getjob_old_nopay_ts <- ts(ages_getjob_old_nopay)
plot.ts(ages_getjob_old_nopay_ts)

###?????м? 
##?????? ???? ????????
#?Ǿ ????(???????? ???????̱⿡ ?Ǿ�� ??????)
sex_getjob_corr <- read_xlsx("/sex_getjob.xlsx", sheet = 1)
View(sex_getjob_corr)
cov(sex_getjob_corr) #?л?
cor(sex_getjob_corr, method = "pearson") #????????

