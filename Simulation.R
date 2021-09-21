#PSC404 Recitation Sept.22
#Simulations in R
#Weihong Qi (Erika)

rm(list=ls()) #clear work space
library(ggplot2)
library(poliscidata)

#Introduction to random number generations
#Generate normal distributed data
sim <- rnorm(100)
sim <- rnorm(1000)
sim <- rnorm(10000)
mean(sim)
summary(sim)

#The data generating process is not actually random
#it depends on the seed you use

set.seed(123)
set.seed(404)
set.seed(101)

sim<- rnorm(100)
sim<- rnorm(1000,5,2)
sim.d<- dnorm(0.5)
sim.p <- pnorm(0.7)
sim.q <- qnorm(0.75)

mean(sim)

#Generate Uniform distributed data
uf <- runif(100)
uf.1 <- runif(100, min=1, max = 3)
mean(uf.1)

#Plot the histogram of simulated data
hist(sim)

ggplot(data.frame(sim), aes(sim))+ geom_histogram(aes(y=..density..)) +
  stat_function(fun=dnorm, args = list(mean=mean(sim), sd=sd(sim)),
                geom = "line", color="red")

#some "real world" examples
sample(c("H","T"), size = 10, replace = TRUE)

sample(1:6, size=10, replace=TRUE, prob = c(1,1,1,2,2,2))

#suppose there is p=0.05 for an individual between 18-65 years old to be infected by COVID
#and p=0.12 for individuals under 18, and p=0.15 for those above 65 years old to be infected
#Let the proportion of people under 18 to be 0.2, above 65 to be 0.25
#Simulate how many people infected in this world

population <- sample(c("under18", "18to65", "over65"), size = 10000, 
                     replace=TRUE, prob = c(0.2,0.55,0.25))
covid <- rep(NA, 10000)

for(i in 1:10000){
  if(population[i]=="unnder18"){
    covid[i] <- rbinom(1,1,0.12)}
    else if(population[i]=="18to65"){
        covid[i] <- rbinom(1,1,0.05)}
    else if(population=="over65"){
        covid[i] <- rbinom(1,1,0.15)}
}
summary(covid)
sum(covid)

#Simulation using dataset
#data from the General Social Survey polls
#we are going to randomly draw observations from the dataset
gss <-gss
index <- seq_len(nrow(gss)) #sequence generation
samp <- sample(index, 5)
gss[samp, "age"]
gss[samp, 9:12]


#Simulate linear model
x <- rnorm(1000)
e<-  rnorm(1000,0,5)

y = 0.5 + 3*x +e
summary(y)
plot(x,y)
abline(lm(y ~x), col="red")
