##############################################################################################
###### Projections of Macroeconomic Variables  ###############################################
##############################################################################################
# Set the working directory

rm(list=ls())
#setwd("insert_directory")

##############################################################################################
# Example 1

rm(list=ls())
mydata<-read.csv ("gtemp.csv")
gtemp<-mydata$"gtem"
plot(gtemp, type="o", ylab="Global Temperature Deviations")
t<-1:142
summary(reg <- lm(gtemp ~ t))
plot(gtemp, ylab="Global Temperature Deviations")
abline(reg,col=3)
#alternatively
line_reg<-reg$fitted.values
lines(line_reg,col=2)
##############################################################################################
reg1= lm(gtemp~time(gtemp), na.action=NULL) # regress gtemp on time
summary(reg1)
#altenatively
#reg2= lm(gtemp~t, na.action=NULL) # regress gtemp on time
plot(gtemp)
abline(reg1)
par(mar=c(1,1,1,1)) # set margin
par(mfrow=c(2,1))
plot(resid(reg1), type="o", main="detrended")
#alternatively
#RES<-reg1$residuals
#plot(RES)
plot(diff(gtemp), type="o", main="first difference")
##############################################################################################
ts.plot(resid(reg1), main="detrended y first difference", xlim=c(0, 130), ylim=c(-0.5, 0.5))
lines(diff(gtemp), lwd=2, col=2)
plot(resid(reg1), main="detrended y first difference", xlim=c(0, 130), ylim=c(-0.5, 0.5))

##############################################################################################
par(mfrow=c(3,1)) # plot ACFs
acf(gtemp, 48, main="gtemp")
acf(resid(reg), 48, main="detrended")
acf(diff(gtemp), 48, main="first difference")
##############################################################################################


#stationarity analysis
# Dickey-Fuller test
library(tseries)

adf.test(gtemp,k=1)
adf.test(resid(reg1))
adf.test(diff(gtemp))
adf.test((gtemp))

summary(resid(reg1))
summary(diff(gtemp))
par(mfrow=c(1,1))
qqnorm(resid(reg1))
#add line
qqline(resid(reg1))
qqnorm(diff(gtemp))
qqline(diff(gtemp))
# normality test
shapiro.test(resid(reg1))
shapiro.test(diff(gtemp))
# Null hypothesis is accepted (both cases)


##############################################################################################
#Example 2
# ARDL model
#install.packages('dynlm')
library(dynlm)

##############################################################################################
Okun<-read.csv("Okun.csv")

g <- ts(Okun$G, start=c(1948,1), frequency=4)
u <- ts(Okun$U, start=c(1948,1), frequency=4) 
par(mar=c(2,4,2,1))
ts.plot(g, col='blue', ylab='Growth')
ts.plot(u, col='red', ylab='∆ Unemployment Rate')
##############################################################################################
#moving average decomposition
g_Comp <- decompose(g)
plot(g_Comp)
g_seas_adj<-g-g_Comp$seasonal
plot.ts(g)
plot.ts(g_seas_adj)
par(mfrow=c(1,1))
plot.ts(g)
lines(g_seas_adj,col=2)
##############################################################################################
#install.packages('quantmod')
library(quantmod)

#install.packages('mFilter')
library(mFilter)

#Gross Domestic Product (GDP)
getSymbols('GDP',src='FRED')
plot(GDP)

hp.decom <- hpfilter(GDP, freq = 1600, type = "lambda")

ts.plot(hp.decom$trend)
lines(hp.decom$x,col=2)
ts.plot(hp.decom$cycle)

##############################################################################################
#chages_unemployement prop grow
du=diff(u)
plot(du)
okun0<-lm(du~g[2:length(g)]) #0lags
okun1<-lm(du~g[2:length(g)]+g[1:length(g)-1]) #1lag

par(mar=c(4,5,1,1))
plot(as.numeric(du))
summary(okun1)
lines(okun0$fitted.values,col=2)
#as time series plot()
ok_fit <- ts(okun0$fitted.values, start=c(1948,2), frequency=4) # time-series object for residuals
plot((du))
lines(ok_fit,col=2)

# G vs du
par(mar=c(5,4,1,1))

plot(as.numeric(du),g[2:length(g)],ylab='Growth',xlab=expression(paste(Delta,"y")))
lines(okun0$fitted.values,g[2:length(g)],col=2)

okun.lag1 <-dynlm(d(u, 1) ~ L(g, 0:1))
summary(okun.lag1)


okun.lag2 <- dynlm(d(u, 1) ~ L(g, 0:2))
okun.lag3 <- dynlm(d(u, 1) ~ L(g, 0:3)) 
summary(okun.lag2)
summary(okun.lag3)

########################3

# analyze just some part of the data

g1=(g[1:50]) # first fifty values
plot(g1)
G1<-ts(g1,start=c(1948,1), frequency=4) #in time series format
plot(G1)
# if we have a time series object we can also use the following syntax:
g2=g[time(g) <= '1952-04-01']
plot(g2)
G2=ts(g2,start=c(1948,1), frequency=4)
plot(G2)
