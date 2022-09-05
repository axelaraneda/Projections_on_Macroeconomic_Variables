
setwd("~/Desktop/Teaching/Macro") #set working directory

rm(list=ls()) #Clear memory
# Charging our libraries 
library(TTR)
library(quantmod)
#################
#import GDP Data
getSymbols('GDP',src='FRED')
plot(GDP)
# We test now for Random-Walk with form: 
# i) GDP(t)=GDP(t-1)+e(t); e(t)~N(0,1); RW without drift
# ii) GDP(t)=GDP(t-1)+a+be(t)=GDP(t-1)+e1(t); e1(t)~N(a,b) RW with drift a and volatility b
# or equivalently using D.GDP(t)=GDP(t)-GDP(t-1):
# D.GDP(t)=e1(t)
D.GDP<-diff(GDP) #Increments
plot(D.GDP)
# we can also plot only a part of the time series
plot(GDP[time(GDP) >= '1950-01-01' & time(GDP) < '2011-01-01'], main("GDP(1950-2010)"))
plot(D.GDP[time(D.GDP) >= '1950-01-01' & time(D.GDP) < '2011-01-01'])

#or equivalently defining a new variable:
D.GDP.50.1999<-D.GDP[time(D.GDP) >= '1950-01-01' & time(D.GDP) < '2000-01-01']
plot(D.GDP.50.1999)

mean(D.GDP,na.rm=TRUE)
#testing for random walk
summary(D.GDP)
#mean different than zero, implies trend: a=mean(D.GDP,na.rm=TRUE)
#variance different than 1, implies b=var(D.GDP,na.rm=TRUE)
plot(density(D.GDP,na.rm = TRUE))
#is the above distribution normal?
#graphical inspection
#qqtest
qqnorm(D.GDP)
qqline(D.GDP)

# we can compare against a true normal distribution with mean a and variance b, for example
uu=rnorm(6155, mean = mean(D.GDP,na.rm = TRUE), sd = sd(D.GDP,na.rm = TRUE))
plot(density(uu,na.rm = TRUE))
plot(density(D.GDP,na.rm = TRUE))
pp=(density(uu))
lines(pp$x,pp$y)
boxplot(uu,D.GDP)

#Hypothesis test, for example Kolmogorov-Smirnov
ks.test(D.GDP, 'pnorm',mean=mean(D.GDP,na.rm = TRUE),sd=sd(D.GDP,na.rm = TRUE))
help("ks.test")
#null hypothesis of equal distribution is rejected.
# then increments are non-normal

