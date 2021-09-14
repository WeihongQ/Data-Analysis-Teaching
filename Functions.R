#PSC404 Recitation 3
#September 13
#Weihong Qi (Erika)

#Introduction to functions
#basic function, condition function, calculate means
#loop function: apply, taaply, lapply, mapply
#optimization functions

rm(list=ls())
library(foreign) #load version 5-12 .dta file
library(haven) # load higher version .dta file
library(datasets)

#########################
#the most simple function
add <- function(x,y){
  x+y
}
add(3,7)

#you can generate a function with conditions
cond <- function(x){
  w <- x<0
  w
  #x-w
  #x[w]
}

cond(3)
cond(-5)

#add default value to function arguments to avoid errors
def <- function(x,y=10){
  w <- x>y
  x-w
}

def(12)
def(1)
def(7,3)

#calculate the mean of all columns or rows in a dataset
cmean <- function(x, removeNA = FALSE){
  m <- numeric(ncol(x))
  for(i in 1:ncol(x)){
    m[i] = mean(x[, i], na.rm = removeNA)
  }
  m
}

earn <- read_dta("CountryOccupationEarnings.dta")
new <- cbind(earn$EarningsGross, earn$EarningsNet, earn$WageGross, earn$WageNet)
cmean(new)
cmean(new, removeNA = TRUE)

##############################################
#Loop functions: apply, lapply, mapply, tapply

#the apply function returns a vector or array or list of values obtained by applying
#a function to margins of an array or matrix

#what is an array?

v1 <- c(1,2,3)
v2 <- c(10,11,12,13)

r<- array(c(v1,v2),dim = c(3,4,2)) #create an array with 2 matrix, each has 3 rows and 3 columns

#name columns (usually the variable name), rows and matrix
rname <- c("row1", "row2", "row3")
cname <- c("column1", "column2", "column3", "column4")
mxname <- c("matrix1", "matrix2")

r<- array(c(v1,v2),dim = c(3,4,2), dimnames = list(rname,cname,mxname))

str(apply)
#MARGIN is the integer identifies which dimension is retained
apply(r,2,mean) #calculate the mean of each column in r
apply(r,1,sum) #sum of the rows

set.seed(123)
x <- matrix(rnorm(100), 10,10)
apply(x,2,quantile)

#the lapply function loops along a list and retirn a list as the same length X
#each of the element is the result of applying specified function
str(lapply)
lapply(x,mean)

l <- list(a=rnorm(100, sd=3), b=(1:10), c=rnbinom(20,5,0.2))
lapply(l,mean)

k <- (1:5)
?runif
lapply(k,runif)

#mapply is a multivariate version of lapply
#this function apply the function to each of the element of each argument

str(mapply)
?rep
list.m <- list(rep(1,3), rep(3,1), rep(2,2))

mapply(rep, 1:3, 3)
mapply(rep, 1:3, 1:3)

#tapply
#tapply is a function to each cell of a ragged array that is
#to each group of values given by a unique combination of the levels of certain factors

str(tapply)
iris <- data(iris)
summary(iris)

tapply(iris$Sepal.Length, iris$Species, mean)


