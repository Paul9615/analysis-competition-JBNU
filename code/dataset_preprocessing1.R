#�ʿ���Ű�� ���̺귯�� ����
library(tidyverse)
library(installr)
library(dplyr)
library(ggplot2)
library(haven)
library(MASS)
library(foreign)

#�����ͼ��� ������ �Ҵ�
da_2013 <- read.csv("/2013.csv")
da_2014 <- read.csv("/2014.csv")
da_2015 <- read.csv("/2015.csv")
da_2016 <- read.csv("/2016.csv")
da_2017 <- read.csv("/2017.csv")
da_2018 <- read.csv("/2018.csv")
da_2019 <- read.csv("/2019.csv")

#�Ҵ�ƴ��� Ȯ��
View(da_2013)
View(da_2014)
View(da_2015)
View(da_2016)
View(da_2017)
View(da_2018)
View(da_2019)

#�� ������ ����Ȯ��
dim(da_2013)
dim(da_2014)
dim(da_2015)
dim(da_2016)
dim(da_2017)
dim(da_2018)
dim(da_2019)

#�����跮 ����
summary(da_2013)
summary(da_2014)
summary(da_2015)
summary(da_2016)
summary(da_2017)
summary(da_2018)
summary(da_2019)

#����ġ ��ó��(����⵵, ) 
#2013�⵵ ������ 
boxplot(da_2013$����⵵) #�̻�ġ ����
boxplot(da_2013$����)
boxplot(da_2013$Ȱ������)#�̻�ġ ����
boxplot(da_2013$�������)
boxplot(da_2013$�������)
boxplot(da_2013$��������)
boxplot(da_2013$�����ñ�)
boxplot(da_2013$�����������׸�������) #���� ����
boxplot(da_2013$����Ȱ����Ȱ������.����.�׸�) #���� ����
boxplot(da_2013$�����ڱԸ�)
boxplot(da_2013$���������)
#2013 ����ġ ó��
summary(da_2013$����⵵)
da_2013$����⵵ <- ifelse(da_2013$����⵵<1963, NA, da_2013$����⵵)
summary(da_2013$Ȱ������)
da_2013$Ȱ������ <- ifelse(da_2013$Ȱ������>6, NA, da_2013$Ȱ������)
da_2013$������� <- ifelse(da_2013$�������==0, NA, da_2013$�������)

#2014
boxplot(da_2014$����⵵) #�̻�ġ ����
boxplot(da_2014$����)
boxplot(da_2014$Ȱ������)#�̻�ġ ����
boxplot(da_2014$�������)
boxplot(da_2014$�������)
boxplot(da_2014$��������)
boxplot(da_2014$�����ñ�)
boxplot(da_2014$�����������׸�������) #���� ����
boxplot(da_2014$����Ȱ����Ȱ������.����.�׸�)
boxplot(da_2014$�����ڱԸ�)
boxplot(da_2014$���������)
#2014 ����ġ ó��
summary(da_2014$����⵵)
da_2014$����⵵ <- ifelse(da_2014$����⵵<1963, NA, da_2014$����⵵)
summary(da_2013$Ȱ������)
da_2014$Ȱ������ <- ifelse(da_2014$Ȱ������>6, NA, da_2014$Ȱ������)
da_2014$������� <- ifelse(da_2014$�������==0, NA, da_2014$�������)

#2015
boxplot(da_2015$����⵵) #�̻�ġ ����
boxplot(da_2015$����)
boxplot(da_2015$Ȱ������)#�̻�ġ ����
boxplot(da_2015$�������)
boxplot(da_2015$�������)
boxplot(da_2015$��������)
boxplot(da_2015$�����ñ�)
boxplot(da_2015$�����������׸�������) #���� ����
boxplot(da_2015$����Ȱ����Ȱ������.����.�׸�)
boxplot(da_2015$�����ڱԸ�)
boxplot(da_2015$���������)
#2015 ����ġ ó��
summary(da_2015$����⵵)
da_2015$����⵵ <- ifelse(da_2015$����⵵<1963, NA, da_2015$����⵵)
summary(da_2013$Ȱ������)
da_2015$Ȱ������ <- ifelse(da_2015$Ȱ������>6, NA, da_2015$Ȱ������)
da_2015$������� <- ifelse(da_2015$�������==0, NA, da_2015$�������)

#2016
boxplot(da_2016$����⵵) #�̻�ġ ����
boxplot(da_2016$����)
boxplot(da_2016$Ȱ������)
boxplot(da_2016$�������)
boxplot(da_2016$�������)
boxplot(da_2016$��������)
boxplot(da_2016$�����ñ�)
boxplot(da_2016$�����������׸�������) #���� ����
boxplot(da_2016$����Ȱ����Ȱ������.����.�׸�)
boxplot(da_2016$�����ڱԸ�)
boxplot(da_2016$���������)
#2016 ����ġ ó��
summary(da_2016$����⵵)
da_2016$����⵵ <- ifelse(da_2016$����⵵<1954, NA, da_2016$����⵵)
da_2016$������� <- ifelse(da_2016$�������==0, NA, da_2016$�������)

#2017
boxplot(da_2017$����⵵) #�̻�ġ ����
boxplot(da_2017$����)
boxplot(da_2017$Ȱ������)
boxplot(da_2017$�������)
boxplot(da_2017$�������)
boxplot(da_2017$��������)
boxplot(da_2017$�����ñ�)
boxplot(da_2017$�����������׸�������) #���� ����
boxplot(da_2017$����Ȱ����Ȱ������.����.�׸�)
boxplot(da_2017$�����ڱԸ�)
boxplot(da_2017$���������)
#2017 ����ġ ó��
summary(da_2017$����⵵)
da_2017$����⵵ <- ifelse(da_2017$����⵵<1954, NA, da_2017$����⵵)
da_2017$������� <- ifelse(da_2017$�������==0, NA, da_2017$�������)

#2018
boxplot(da_2018$����⵵) 
boxplot(da_2018$����)
boxplot(da_2018$Ȱ������)#�̻�ġ ����
boxplot(da_2018$�������)
boxplot(da_2018$�������)
boxplot(da_2018$��������)
boxplot(da_2018$�����ñ�)
boxplot(da_2018$�����������׸�������) #���� ����
boxplot(da_2018$����Ȱ����Ȱ������.����.�׸�)
boxplot(da_2018$�����ڱԸ�)
boxplot(da_2018$���������)
#2018 ����ġ ó�� 
da_2018$������� <- ifelse(da_2018$�������==0, NA, da_2018$�������)

#2019
boxplot(da_2019$����⵵) 
boxplot(da_2019$����)
boxplot(da_2019$Ȱ������)
boxplot(da_2019$�������)
boxplot(da_2019$�������)
boxplot(da_2019$��������)
boxplot(da_2019$�����ñ�)
boxplot(da_2019$�����������׸�������) #���� ����
boxplot(da_2019$����Ȱ����Ȱ������.����.�׸�)
boxplot(da_2019$�����ڱԸ�)
boxplot(da_2019$���������)
#2019 ����ġ ó�� 
da_2019$������� <- ifelse(da_2019$�������==0, NA, da_2019$�������)
