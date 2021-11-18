#PSC404 Recitation 
#November 17
#Weihong Qi (Erika)

rm(list=ls()) #clear work space

setwd("/Users/weihong/Desktop/data science/psc404")


library(ggplot2)
library(stargazer)
library(foreign) #load version 5-12 .dta file
library(haven) # load higher version .dta file

cfps <- read_dta("CFPS.dta")

#interaction term
gender.1 <- lm(income ~ age + cfps2018eduy + gender, data=cfps) #male=1
summary(gender.1)

cfps$gender_edu <- cfps$gender*cfps$cfps2018eduy
gender.i <-  lm(income ~ age + cfps2018eduy + gender + gender_edu, data=cfps) 
summary(gender.i)

cfps$gd <-factor(cfps$gender)

ggplot(data=cfps, aes(x=cfps2018eduy, y=income,color=gd))+
    geom_point() +
    geom_smooth(method = "lm") +
    theme_bw()


#quadratic term 
age.1 <- lm(income ~ age + cfps2018eduy + gender, data=cfps)
summary(age.1)

cfps$age_2 <- cfps$age^2

age.2 <-lm(income ~ age + age_2+cfps2018eduy + gender, data=cfps)
summary(age.2)

ggplot(data=cfps, aes(x=age, y=income)) + 
    geom_point()+
    stat_smooth(method='lm', formula=y~poly(x,2))

#log term
cfps[cfps$income<=0,] <-NA
cfps$lg_income <- log(cfps$income)

edu.1 <- lm(income ~ age + cfps2018eduy + gender, data=cfps)
summary(edu.1)

edu.lg<- lm(lg_income ~ age + cfps2018eduy + gender, data=cfps)
summary(edu.lg)

ggplot(data=cfps, aes(x=cfps2018eduy, y=lg_income))+
    geom_point() +
    geom_smooth(method = "lm") +
    theme_bw()


