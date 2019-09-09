#필요패키지 라이브러리 부착
library(tidyverse)
library(installr)
library(dplyr)
library(ggplot2)
library(haven)
library(MASS)
library(foreign)

#데이터셋을 변수에 할당
da_2013 <- read.csv("F:/경진대회/데이터/2013.csv")
da_2014 <- read.csv("F:/경진대회/데이터/2014.csv")
da_2015 <- read.csv("F:/경진대회/데이터/2015.csv")
da_2016 <- read.csv("F:/경진대회/데이터/2016.csv")
da_2017 <- read.csv("F:/경진대회/데이터/2017.csv")
da_2018 <- read.csv("F:/경진대회/데이터/2018.csv")
da_2019 <- read.csv("F:/경진대회/데이터/2019.csv")

#할당됐는지 확인
View(da_2013)
View(da_2014)
View(da_2015)
View(da_2016)
View(da_2017)
View(da_2018)
View(da_2019)

#각 데이터 차원확인
dim(da_2013)
dim(da_2014)
dim(da_2015)
dim(da_2016)
dim(da_2017)
dim(da_2018)
dim(da_2019)

#요약통계량 산출
summary(da_2013)
summary(da_2014)
summary(da_2015)
summary(da_2016)
summary(da_2017)
summary(da_2018)
summary(da_2019)

#결측치 전처리(출생년도, ) 
#2013년도 데이터 
boxplot(da_2013$출생년도) #이상치 있음
boxplot(da_2013$성별)
boxplot(da_2013$활동상태)#이상치 있음
boxplot(da_2013$취업여부)
boxplot(da_2013$취업구분)
boxplot(da_2013$전직유무)
boxplot(da_2013$전직시기)
boxplot(da_2013$직장직장을그만둔이유) #읽지 못함
boxplot(da_2013$경제활경제활동상태.구분.항목) #읽지 못함
boxplot(da_2013$종사자규모)
boxplot(da_2013$종사상지위)
#2013 결측치 처리
summary(da_2013$출생년도)
da_2013$출생년도 <- ifelse(da_2013$출생년도<1963, NA, da_2013$출생년도)
summary(da_2013$활동상태)
da_2013$활동상태 <- ifelse(da_2013$활동상태>6, NA, da_2013$활동상태)
da_2013$취업여부 <- ifelse(da_2013$취업여부==0, NA, da_2013$취업여부)

#2014
boxplot(da_2014$출생년도) #이상치 있음
boxplot(da_2014$성별)
boxplot(da_2014$활동상태)#이상치 있음
boxplot(da_2014$취업여부)
boxplot(da_2014$취업구분)
boxplot(da_2014$전직유무)
boxplot(da_2014$전직시기)
boxplot(da_2014$직장직장을그만둔이유) #읽지 못함
boxplot(da_2014$경제활경제활동상태.구분.항목)
boxplot(da_2014$종사자규모)
boxplot(da_2014$종사상지위)
#2014 결측치 처리
summary(da_2014$출생년도)
da_2014$출생년도 <- ifelse(da_2014$출생년도<1963, NA, da_2014$출생년도)
summary(da_2013$활동상태)
da_2014$활동상태 <- ifelse(da_2014$활동상태>6, NA, da_2014$활동상태)
da_2014$취업여부 <- ifelse(da_2014$취업여부==0, NA, da_2014$취업여부)

#2015
boxplot(da_2015$출생년도) #이상치 있음
boxplot(da_2015$성별)
boxplot(da_2015$활동상태)#이상치 있음
boxplot(da_2015$취업여부)
boxplot(da_2015$취업구분)
boxplot(da_2015$전직유무)
boxplot(da_2015$전직시기)
boxplot(da_2015$직장직장을그만둔이유) #읽지 못함
boxplot(da_2015$경제활경제활동상태.구분.항목)
boxplot(da_2015$종사자규모)
boxplot(da_2015$종사상지위)
#2015 결측치 처리
summary(da_2015$출생년도)
da_2015$출생년도 <- ifelse(da_2015$출생년도<1963, NA, da_2015$출생년도)
summary(da_2013$활동상태)
da_2015$활동상태 <- ifelse(da_2015$활동상태>6, NA, da_2015$활동상태)
da_2015$취업여부 <- ifelse(da_2015$취업여부==0, NA, da_2015$취업여부)

#2016
boxplot(da_2016$출생년도) #이상치 있음
boxplot(da_2016$성별)
boxplot(da_2016$활동상태)
boxplot(da_2016$취업여부)
boxplot(da_2016$취업구분)
boxplot(da_2016$전직유무)
boxplot(da_2016$전직시기)
boxplot(da_2016$직장직장을그만둔이유) #읽지 못함
boxplot(da_2016$경제활경제활동상태.구분.항목)
boxplot(da_2016$종사자규모)
boxplot(da_2016$종사상지위)
#2016 결측치 처리
summary(da_2016$출생년도)
da_2016$출생년도 <- ifelse(da_2016$출생년도<1954, NA, da_2016$출생년도)
da_2016$취업여부 <- ifelse(da_2016$취업여부==0, NA, da_2016$취업여부)

#2017
boxplot(da_2017$출생년도) #이상치 있음
boxplot(da_2017$성별)
boxplot(da_2017$활동상태)
boxplot(da_2017$취업여부)
boxplot(da_2017$취업구분)
boxplot(da_2017$전직유무)
boxplot(da_2017$전직시기)
boxplot(da_2017$직장직장을그만둔이유) #읽지 못함
boxplot(da_2017$경제활경제활동상태.구분.항목)
boxplot(da_2017$종사자규모)
boxplot(da_2017$종사상지위)
#2017 결측치 처리
summary(da_2017$출생년도)
da_2017$출생년도 <- ifelse(da_2017$출생년도<1954, NA, da_2017$출생년도)
da_2017$취업여부 <- ifelse(da_2017$취업여부==0, NA, da_2017$취업여부)

#2018
boxplot(da_2018$출생년도) 
boxplot(da_2018$성별)
boxplot(da_2018$활동상태)#이상치 있음
boxplot(da_2018$취업여부)
boxplot(da_2018$취업구분)
boxplot(da_2018$전직유무)
boxplot(da_2018$전직시기)
boxplot(da_2018$직장직장을그만둔이유) #읽지 못함
boxplot(da_2018$경제활경제활동상태.구분.항목)
boxplot(da_2018$종사자규모)
boxplot(da_2018$종사상지위)
#2018 결측치 처리 
da_2018$취업여부 <- ifelse(da_2018$취업여부==0, NA, da_2018$취업여부)

#2019
boxplot(da_2019$출생년도) 
boxplot(da_2019$성별)
boxplot(da_2019$활동상태)
boxplot(da_2019$취업여부)
boxplot(da_2019$취업구분)
boxplot(da_2019$전직유무)
boxplot(da_2019$전직시기)
boxplot(da_2019$직장직장을그만둔이유) #읽지 못함
boxplot(da_2019$경제활경제활동상태.구분.항목)
boxplot(da_2019$종사자규모)
boxplot(da_2019$종사상지위)
#2019 결축치 처리 
da_2019$취업여부 <- ifelse(da_2019$취업여부==0, NA, da_2019$취업여부)
