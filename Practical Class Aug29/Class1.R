#https://statsandr.com/


setwd("~/Desktop/Teaching/Macro") #set working directory

rm(list=ls()) #Clear memory

#install.packages("readxl") #read excel files
library("readxl") #load library
mydata2<-read_excel ("indicador.xls") #Inflation (CHILE)


mydata=read.csv ("gtemp.csv")
plot(mydata, type="l", ylab="Global Temperature Deviations")
temp<-mydata$gtem


plot(mydata2, type="o", ylab=expression(paste( Delta, " IPC (Inflation)")))


min(temp)
max(temp)
range(mydata$gtem)
rng=range(temp)
rng[2]
#median, mean, quantile,sd,var
quantile(temp)
quantile(temp, 0.8) #any percentile 
var(temp)


temp=mydata$gtem
ipc=mydata2$Valor

boxplot(temp)
hist(temp)
plot(density(temp))

install.packages("tseries")


findata<-read_excel("GSPC.xlsx")
price<-findata$adjclose
lnprice<-log(price)
plot(price, type="o", ylab="S&P 500")
date=findata$Date
DATE=as.Date(date,format="%m/%d/%Y")
plot(DATE,lnprice, type="o", ylab="Log S&P 500")
logret<-diff(lnprice)
plot(DATE[2:8228],logret, type="o", ylab="S&P 500")


###############

# Obtaining Macroeconomic data

#install.packages('quantmod')
#install.packages('xts')
#install.packages('zoo')
#install.packages('TTR')
library(TTR)
library(quantmod)

rm(list=ls())
getSymbols('CPALTT01USQ657N',src = 'FRED')

cpi.changes <- CPALTT01USQ657N     
head(cpi.changes) #to see the first values
tail(cpi.changes) #to see the last values
plot(cpi.changes,main='CPI',ylab='%',xlab='Time')

getSymbols('UNRATE',src = 'FRED')
summary(UNRATE)
head(UNRATE)
plot(UNRATE,main='Unemployment Rate',ylab='%',xlab='Time')

getSymbols('DEXUSEU',src='FRED')
plot(DEXUSEU)
