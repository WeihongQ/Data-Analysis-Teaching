#PSC404 Recitation 1
#September 1
#Weihong Qi (Erika)

#introduction to libraries
#how to import dataset
#missing data
#preliminary data analysis: summary statistics, figures, correlation analysis


rm(list=ls()) #clear work space
ls()

#find the directory, reset the directory
getwd()

setwd("/Users/weihong/Desktop/data science/psc404")

#introduction to libraries
install.packages("poliscidata")
install.packages("ggpubr")


library(ggplot2)
library(stargazer)
library(foreign) #load version 5-12 .dta file
library(haven) # load higher version .dta file
library(dplyr) #data manipulation tools
library(ggpubr)
library(poliscidata)

#how to load dataset and simple data analysis
#(1) use dataset from packages
gss <-gss #the General Social Survey polls, individual attitudes and beliefs
summary(gss$age)

m <- gss %>% group_by(pres08) %>% summarise(Freq=n())
n <- gss %>% group_by(zodiac) %>% summarise(Freq=n())
ggplot(data=m, aes(x=pres08, y=Freq)) +geom_bar(stat = 'identity')
ggplot(data=n, aes(x=zodiac, y=Freq)) +geom_bar(stat = 'identity')

freqC(x=gss$zodiac, w=gss$childs)

graphics.off() #clear plots

#(2) use dataset from website
#read the autocratic ruling parties dataset
party <- read.dta("http://sites.google.com/site/mkmtwo/AutocraticRulingPartiesDataset.dta")

plot(x=party$Origin, y=party$Year_in_Power, na.rm=TRUE)

counts <- table(party$Year_in_Power)
barplot(counts, main="Year in power")

party_complete= party[complete.cases(party$Origin), ]

ggplot(data=party_complete, mapping= aes(x=party_complete$Origin, y=party_complete$Year_in_Power)) +geom_point()

#(3)use local dataset 
earn <- read_dta("CountryOccupationEarnings.dta")

ggplot(earn, aes(x=Taxes)) + geom_density()

res <- cor.test(earn$Hours, earn$EarningsNet, 
                method = "pearson")
res

ggplot(earn, aes(x = Hours, y = EarningsNet) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#print summary statistics
stargazer(earn)

#print regression results 
coef.1 <- lm(EarningsNet ~ Hours, data= earn)
summary(coef.1)

coef.2<- lm(EarningsNet ~ Hours + Population, data= earn)
stargazer(coef.1, coef.2, title="Results", align=TRUE)

