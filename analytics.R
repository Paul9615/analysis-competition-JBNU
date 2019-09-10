install.packages("tseries")
install.packages("forecast")
install.packages("TTR")
install.packages("Hmisc")
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

###시계열분석 
##성별에 따른 취업여부 
#did female
sex_getjob_did_fe <- read_xlsx("C:/Users/user/Desktop/sex_getjob.xlsx", sheet=2)
sex_getjob_did_fe_ts <- ts(sex_getjob_did_fe)
plot.ts(sex_getjob_did_fe_ts)

#did male
sex_getjob_did_ma <- read_xlsx("C:/Users/user/Desktop/sex_getjob.xlsx", sheet=3)
sex_getjob_did_ma_ts <- ts(sex_getjob_did_ma)
plot.ts(sex_getjob_did_ma_ts)

#nopay female
sex_getjob_nopay_fe <- read_xlsx("C:/Users/user/Desktop/sex_getjob.xlsx", sheet=6)
sex_getjob_nopay_fe_ts <- ts(sex_getjob_nopay_fe)
plot.ts(sex_getjob_nopay_fe_ts)

#nopay male
sex_getjob_nopay_ma <- read_xlsx("C:/Users/user/Desktop/sex_getjob.xlsx", sheet=7)
sex_getjob_nopay_ma_ts <- ts(sex_getjob_nopay_ma)
plot.ts(sex_getjob_nopay_ma_ts)

##성별에 따른 취업여부
#mid did
ages_getjob_mid_did <- read_xlsx("C:/Users/user/Desktop/ages_getjob.xlsx", sheet=2)
ages_getjob_mid_did_ts <- ts(ages_getjob_mid_did)
plot.ts(ages_getjob_mid_did_ts)

#mid nopay 
ages_getjob_mid_nopay <- read_xlsx("C:/Users/user/Desktop/ages_getjob.xlsx", sheet=4)
ages_getjob_mid_nopay_ts <- ts(ages_getjob_mid_nopay)
plot.ts(ages_getjob_mid_nopay_ts)

#young did 
ages_getjob_young_did <- read_xlsx("C:/Users/user/Desktop/ages_getjob.xlsx", sheet=5)
ages_getjob_young_did_ts <- ts(ages_getjob_young_did)
plot.ts(ages_getjob_young_did_ts)

#young nopay
ages_getjob_young_nopay <- read_xlsx("C:/Users/user/Desktop/ages_getjob.xlsx", sheet=7)
ages_getjob_young_nopay_ts <- ts(ages_getjob_young_nopay)
plot.ts(ages_getjob_young_nopay_ts)

#old did 
ages_getjob_old_did <- read_xlsx("C:/Users/user/Desktop/ages_getjob.xlsx", sheet=8)
ages_getjob_old_did_ts <- ts(ages_getjob_old_did)
plot.ts(ages_getjob_old_did_ts)

#old nopay
ages_getjob_old_nopay <- read_xlsx("C:/Users/user/Desktop/ages_getjob.xlsx", sheet=9)
ages_getjob_old_nopay_ts <- ts(ages_getjob_old_nopay)
plot.ts(ages_getjob_old_nopay_ts)

###상관분석 
##성별에 따른 취업여부
#피어슨 사용(변수들이 연속형이기에 피어슨을 사용함)
sex_getjob_corr <- read_xlsx("C:/Users/user/Desktop/analytics contest/preprocessing data/sex_getjob.xlsx", sheet = 1)
View(sex_getjob_corr)
cov(sex_getjob_corr) #분산
cor(sex_getjob_corr, method = "pearson") #상관관계

##연령대에 따른 취업여부
#피어슨 사용(변수들이 연속형이기에 피어슨을 사용함)
age_getjob_corr <- read_xlsx("C:/Users/user/Desktop/analytics contest/preprocessing data/ages_getjob.xlsx", sheet=1)
View(age_getjob_corr)
cov(age_getjob_corr)
cor(age_getjob_corr, method = "pearson")

## 성별과 연령별 상관관계
#위와 같이 피어슨 사용
#데이터는 preprocessing한 column data들을 열로 모두 변환하여 정렬한 데이터임
sex_getjob_corr_1 <- c(10333,	10202,	17597,	17897,	18632,	76839,	44307, 5071,	4995,	2517,	2456,	2567,	94120,	54404, 3541,	3877,	199731,	195102,	195854,	232029,	133439, 738,	838,	103961,	101752,	103904,	224137,	129145)
age_getjob_corr_1 <- c(3596,	3422,	10934, 10546,	10438,	62247,	37560, 1072,	1219,	99070,	94014,	90947,	275270,	152710, 3909,	4018,	986,	901,	824,	106238,	59641, 428,	504,	78264,	74360,	73323,	4059,	2106)
cor.test(sex_getjob_corr_1, age_getjob_corr_1, method = "pearson")

###regression 
#Y=연령 X=성별
sex_getjob_reg <- c(10333,	10202,	17597,	17897,	18632,	76839,	44307, 5071,	4995,	2517,	2456,	2567,	94120,	54404, 3541,	3877,	199731,	195102,	195854,	232029,	133439, 738,	838,	103961,	101752,	103904,	224137,	129145)
age_getjob_reg <- c(3596,	3422,	10934, 10546,	10438,	62247,	37560, 1072,	1219,	99070,	94014,	90947,	275270,	152710, 3909,	4018,	986,	901,	824,	106238,	59641, 428,	504,	78264,	74360,	73323,	4059,	2106)
lm(age_getjob_reg~sex_getjob_reg)
summary(lm(age_getjob_reg~sex_getjob_reg))
#Y=성별 X=연령 
lm(sex_getjob_reg~age_getjob_reg)
summary(lm(sex_getjob_reg~age_getjob_reg))
