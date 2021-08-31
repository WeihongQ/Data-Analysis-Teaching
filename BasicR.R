#PSC404 Recitation 1
#September 1
#Weihong Qi (Erika)

rm(list=ls()) #clear work space
ls()

#find the directory, reset the directory
getwd()

setwd("/Users/weihong/Desktop/data science/psc404")

#introduction to libraries
install.packages("poliscidata")


library(poliscidata)
library(ggplot2)
library(stargazer)
library(foreign) #load version 5-12 .dta file
library(haven) # load higher version .dta file
library(dplyr) #data manipulation tools

#how to load dataset and simple data analysis
#(1) use dataset from packages
gss <-gss #the General Social Survey polls, individual attitudes and beliefs

freqC(x=gss$zodiac, w=gss$childs)

graphics.off() #clear plots

#(2) use dataset from website
#read the autocratic ruling parties dataset
party <- read.dta("http://sites.google.com/site/mkmtwo/AutocraticRulingPartiesDataset.dta")
party$duration= party$Last_Year_of_Power - party$Year_in_Power

counts <- table(party$Year_in_Power)
barplot(counts, main="years")

party_sub <- na.omit(party$Origin)
party_sub <- data.frame(party_sub)

party_complete= party[complete.cases(party$Origin), ]

ggplot(data=party_complete, mapping= aes(x=party_complete$Origin, y=party_complete$Year_in_Power)) +geom_point()

plot(x=party$Origin, y=party$Year_in_Power, na.rm=TRUE)

#(3)use local dataset 
earn <- read_dta("CountryOccupationEarnings.dta")

ggplot(earn, aes(x=Taxes)) + geom_density()


