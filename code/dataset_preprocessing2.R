library(tidyverse)
library(installr)
library(dplyr)
library(ggplot2)
library(haven)
library(MASS)
library(foreign)

#������� ������ ���� ��ȯ �� �߰� (did=�־���, nopay=���ް���������, didnt=������)
da_2013 <- da_2013 %>% 
  mutate(gjob = ifelse(�������==1, "did",
                       ifelse(�������==2, "nopay", "didnt")))

da_2014 <- da_2014 %>% 
  mutate(gjob = ifelse(�������==1, "did",
                           ifelse(�������==2, "nopay", "didnt")))

da_2015 <- da_2015 %>% 
  mutate(gjob = ifelse(�������==1, "did",
                           ifelse(�������==2, "nopay", "didnt")))

da_2016 <- da_2016 %>% 
  mutate(gjob = ifelse(�������==1, "did",
                           ifelse(�������==2, "nopay", "didnt")))

da_2017 <- da_2017 %>% 
  mutate(gjob = ifelse(�������==1, "did",
                           ifelse(�������==2, "nopay", "didnt")))

da_2018 <- da_2018 %>% 
  mutate(gjob = ifelse(�������==1, "did",
                           ifelse(�������==2, "nopay", "didnt")))

da_2019 <- da_2019 %>% 
  mutate(gjob = ifelse(�������==1, "did",
                           ifelse(�������==2, "nopay", "didnt")))

#���� ������ ���� ��ȯ �� �߰� 
da_2013 <- da_2013 %>% 
  mutate(sex = ifelse(����==1, "male", "female"))

da_2014 <- da_2014 %>% 
  mutate(sex = ifelse(����==1, "male", "female")) 

da_2015 <- da_2015 %>% 
  mutate(sex = ifelse(����==1, "male", "female"))

da_2016 <- da_2016 %>% 
  mutate(sex = ifelse(����==1, "male", "female"))

da_2017 <- da_2017 %>% 
  mutate(sex = ifelse(����==1, "male", "female"))

da_2018 <- da_2018 %>% 
  mutate(sex = ifelse(����==1, "male", "female"))

da_2019 <- da_2019 %>% 
  mutate(sex = ifelse(����==1, "male", "female"))

#������ ������ ���� �������
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

#û�� �� �߳� ������� 
#����⵵�� ���̷� ����
da_2013$����⵵ <- 2013 - da_2013$����⵵+1
da_2014$����⵵ <- 2014 - da_2014$����⵵+1
da_2015$����⵵ <- 2015 - da_2015$����⵵+1
da_2016$����⵵ <- 2016 - da_2016$����⵵+1
da_2017$����⵵ <- 2017 - da_2017$����⵵+1
da_2018$����⵵ <- 2018 - da_2018$����⵵+1
da_2019$����⵵ <- 2019 - da_2019$����⵵+1
summary(da_2013$����⵵)
summary(da_2014$����⵵)
summary(da_2015$����⵵)
summary(da_2016$����⵵)
summary(da_2017$����⵵)
summary(da_2018$����⵵)
summary(da_2019$����⵵)

#���ɺ��� �з�(30�� �̸� �ʳ�, 30~59�� �߳� �� ���) 
da_2013 <- da_2013 %>%
  mutate(ages = ifelse(����⵵ < 30, "young",
                              ifelse(����⵵ <= 59, "middle", "old")))
table(da_2013$ages)

da_2014 <- da_2014 %>%
  mutate(ages = ifelse(����⵵ < 30, "young",
                           ifelse(����⵵ <= 59, "middle", "old")))
table(da_2014$ages)

da_2015 <- da_2015 %>%
  mutate(ages = ifelse(����⵵ < 30, "young",
                           ifelse(����⵵ <= 59, "middle", "old")))
table(da_2015$ages)

da_2016 <- da_2016 %>%
  mutate(ages = ifelse(����⵵ < 30, "young",
                           ifelse(����⵵ <= 59, "middle", "old")))
table(da_2016$ages)

da_2017 <- da_2017 %>%
  mutate(ages = ifelse(����⵵ < 30, "young",
                           ifelse(����⵵ <= 59, "middle", "old")))
table(da_2017$ages)

da_2018 <- da_2018 %>%
  mutate(ages = ifelse(����⵵ < 30, "young",
                           ifelse(����⵵ <= 59, "middle", "old")))
table(da_2018$ages)

da_2019 <- da_2019 %>%
  mutate(ages = ifelse(����⵵ < 30, "young",
                           ifelse(����⵵ <= 59, "middle", "old")))
table(da_2019$ages)

#���ɺ� ������� 
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