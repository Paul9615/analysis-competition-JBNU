library(tidyverse)
library(installr)
library(dplyr)
library(ggplot2)
library(haven)
library(MASS)
library(foreign)

#취업여부 데이터 변수 변환 및 추가 (did=있었음, nopay=무급가족종사자, didnt=없었다)
da_2013 <- da_2013 %>% 
  mutate(gjob = ifelse(취업여부==1, "did",
                       ifelse(취업여부==2, "nopay", "didnt")))

da_2014 <- da_2014 %>% 
  mutate(gjob = ifelse(취업여부==1, "did",
                           ifelse(취업여부==2, "nopay", "didnt")))

da_2015 <- da_2015 %>% 
  mutate(gjob = ifelse(취업여부==1, "did",
                           ifelse(취업여부==2, "nopay", "didnt")))

da_2016 <- da_2016 %>% 
  mutate(gjob = ifelse(취업여부==1, "did",
                           ifelse(취업여부==2, "nopay", "didnt")))

da_2017 <- da_2017 %>% 
  mutate(gjob = ifelse(취업여부==1, "did",
                           ifelse(취업여부==2, "nopay", "didnt")))

da_2018 <- da_2018 %>% 
  mutate(gjob = ifelse(취업여부==1, "did",
                           ifelse(취업여부==2, "nopay", "didnt")))

da_2019 <- da_2019 %>% 
  mutate(gjob = ifelse(취업여부==1, "did",
                           ifelse(취업여부==2, "nopay", "didnt")))

#성별 데이터 변수 변환 및 추가 
da_2013 <- da_2013 %>% 
  mutate(sex = ifelse(성별==1, "male", "female"))

da_2014 <- da_2014 %>% 
  mutate(sex = ifelse(성별==1, "male", "female")) 

da_2015 <- da_2015 %>% 
  mutate(sex = ifelse(성별==1, "male", "female"))

da_2016 <- da_2016 %>% 
  mutate(sex = ifelse(성별==1, "male", "female"))

da_2017 <- da_2017 %>% 
  mutate(sex = ifelse(성별==1, "male", "female"))

da_2018 <- da_2018 %>% 
  mutate(sex = ifelse(성별==1, "male", "female"))

da_2019 <- da_2019 %>% 
  mutate(sex = ifelse(성별==1, "male", "female"))

#연도별 성별에 따른 취업여부
sex_job_13 <- da_2013 %>%
  filter(!is.na(gjob)) %>%
  group_by(gjob, sex) %>%
  summarise(n=n())
sex_job_13

sex_job_14 <- da_2014 %>% 
  filter(!is.na(gjob)) %>%
  group_by(gjob, sex) %>%
  summarise(n=n())
sex_job_14

sex_job_15 <- da_2015 %>% 
  filter(!is.na(gjob)) %>%
  group_by(gjob, sex) %>%
  summarise(n=n())
sex_job_15

sex_job_16 <- da_2016 %>% 
  filter(!is.na(gjob)) %>%
  group_by(gjob, sex) %>%
  summarise(n=n())
sex_job_16

sex_job_17 <- da_2017 %>% 
  filter(!is.na(gjob)) %>%
  group_by(gjob, sex) %>%
  summarise(n=n())
sex_job_17

sex_job_18 <- da_2018 %>%
  filter(!is.na(gjob)) %>%
  group_by(gjob, sex) %>%
  summarise(n=n())
sex_job_18

sex_job_19 <- da_2019 %>% 
  filter(!is.na(gjob)) %>%
  group_by(gjob, sex) %>%
  summarise(n=n())
sex_job_19

#청년 및 중년 취업여부 
#출생년도를 나이로 변경
da_2013$출생년도 <- 2013 - da_2013$출생년도+1
da_2014$출생년도 <- 2014 - da_2014$출생년도+1
da_2015$출생년도 <- 2015 - da_2015$출생년도+1
da_2016$출생년도 <- 2016 - da_2016$출생년도+1
da_2017$출생년도 <- 2017 - da_2017$출생년도+1
da_2018$출생년도 <- 2018 - da_2018$출생년도+1
da_2019$출생년도 <- 2019 - da_2019$출생년도+1
summary(da_2013$출생년도)
summary(da_2014$출생년도)
summary(da_2015$출생년도)
summary(da_2016$출생년도)
summary(da_2017$출생년도)
summary(da_2018$출생년도)
summary(da_2019$출생년도)

#연령별로 분류(30세 미만 초년, 30~59세 중년 외 노년) 
da_2013 <- da_2013 %>%
  mutate(ages = ifelse(출생년도 < 30, "young",
                              ifelse(출생년도 <= 59, "middle", "old")))
table(da_2013$ages)

da_2014 <- da_2014 %>%
  mutate(ages = ifelse(출생년도 < 30, "young",
                           ifelse(출생년도 <= 59, "middle", "old")))
table(da_2014$ages)

da_2015 <- da_2015 %>%
  mutate(ages = ifelse(출생년도 < 30, "young",
                           ifelse(출생년도 <= 59, "middle", "old")))
table(da_2015$ages)

da_2016 <- da_2016 %>%
  mutate(ages = ifelse(출생년도 < 30, "young",
                           ifelse(출생년도 <= 59, "middle", "old")))
table(da_2016$ages)

da_2017 <- da_2017 %>%
  mutate(ages = ifelse(출생년도 < 30, "young",
                           ifelse(출생년도 <= 59, "middle", "old")))
table(da_2017$ages)

da_2018 <- da_2018 %>%
  mutate(ages = ifelse(출생년도 < 30, "young",
                           ifelse(출생년도 <= 59, "middle", "old")))
table(da_2018$ages)

da_2019 <- da_2019 %>%
  mutate(ages = ifelse(출생년도 < 30, "young",
                           ifelse(출생년도 <= 59, "middle", "old")))
table(da_2019$ages)

#연령별 취업여부 
agjob_13 <- da_2013 %>% 
  filter(!is.na(ages) & !is.na(gjob)) %>%
  group_by(ages, gjob) %>% 
  summarise(n=n())
agjob_13

agjob_14 <- da_2014 %>% 
  filter(!is.na(ages) & !is.na(gjob)) %>%
  group_by(ages, gjob) %>% 
  summarise(n=n())
agjob_14

agjob_15 <- da_2015 %>% 
  filter(!is.na(ages) & !is.na(gjob)) %>%
  group_by(ages, gjob) %>% 
  summarise(n=n())
agjob_15

agjob_16 <- da_2016 %>% 
  filter(!is.na(ages) & !is.na(gjob)) %>%
  group_by(ages, gjob) %>% 
  summarise(n=n())
agjob_16

agjob_17 <- da_2017 %>% 
  filter(!is.na(ages) & !is.na(gjob)) %>%
  group_by(ages, gjob) %>% 
  summarise(n=n())
agjob_17

agjob_18 <- da_2018 %>% 
  filter(!is.na(ages) & !is.na(gjob)) %>%
  group_by(ages, gjob) %>% 
  summarise(n=n())
agjob_18

agjob_19 <- da_2019 %>% 
  filter(!is.na(ages) & !is.na(gjob)) %>%
  group_by(ages, gjob) %>% 
  summarise(n=n())
agjob_19
