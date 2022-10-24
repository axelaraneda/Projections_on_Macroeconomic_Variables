setwd("~/Desktop/Teaching/Macro") #set working directory

rm(list=ls()) #Clear memory


# Chilean CPI example
data<-read.csv ("cpi.csv")
cpi <- ts(data[,2],start = c(2013,1), end=c(2018, 6), frequency = 12)
par(mar=c(1,1,1,1))
plot(cpi)
adf.test(cpi, k=1) # non-stationary
#differences
d.cpi<-diff(cpi)
plot(d.cpi)
adf.test(d.cpi, k=1) # p-value<1%; i.e., stationarity

##############################################################################################
# Forecasting and out-of-sample error calculation

#dividing the data
train_series=cpi[1:44]
test_series=cpi[45:62]

arimaModel_1=arima(train_series, order=c(1,1,1))
print(arimaModel_1) 
# Statistical significance of the coefficients
library(lmtest)
coeftest(arimaModel_1)
#ar1 coef. is non-significant
# model 1 is rejected
arimaModel_2=arima(train_series, order=c(1,1,0))
print(arimaModel_2)
coeftest(arimaModel_2)
checkresiduals(arimaModel_2)
arimaModel_3=arima(train_series, order=c(0,1,1))
coeftest(arimaModel_3)
checkresiduals(arimaModel_3)


# in-sample analysis: lower AIC or BIC
#AIC
arimaModel_2$aic 
arimaModel_3$aic
#BIC
BIC(arimaModel_2)
BIC(arimaModel_3)
######

#out of sample analysis
arima2_out<-arima(test_series,order=c(1,1,0),fixed=arimaModel_2$coef) # using the coefficients previously fixed
summary(arima2_out)
accuracy(arima2_out) # same list of errors as previous
arima3_out<-arima(test_series,order=c(0,1,1),fixed=arimaModel_3$coef)

# out of sample based on criteria
arima2_out$aic 
arima3_out$aic
BIC(arima2_out)
BIC(arima3_out)
##########

# Example using auto.arima and financial data
library('quantmod')
getSymbols('^GSPC',src='yahoo', from="2012-01-01",periodicity = 'daily') # S&P500 since 2012
SP500<-GSPC$GSPC.Adjusted
plot(SP500)
adf.test(SP500,k=1)
L.SP500<-log(SP500)
adf.test(L.SP500,k=1)
plot(L.SP500)
r.SP500<-diff(L.SP500)
plot(r.SP500)
adf.test(r.SP500[2:length(r.SP500)],k=1)
d.SP500<-diff(SP500)
adf.test(d.SP500[2:length(r.SP500)],k=1)
plot(d.SP500)

# Prices and log prices are non-stationary 
# while their differences (d.SP500 and r.SP500) are stationary.
# It means that both SP500 and L.SP500 are I(1).

#fit auto.arima
autoarima<-auto.arima(r.SP500)
#library(lmtest)
coeftest(autoarima) # checking the statistical significance
autoarima2<-auto.arima(L.SP500) # Same but using log-prices.
print(autoarima2)

####forecasting

forecast1<-forecast(autoarima,10,level=95) # Predicting 10 values ahead
plot(forecast1,xlim=c(2710,2730))
par(mar=c(4,3,1,1))
length(r.SP500)

forecast2<-forecast(autoarima2,10,level=95)
plot(forecast2,xlim=c(2700,2730),ylim=c(8,8.4))
#########

#Rolling average 
?rollmean
a<-c(4,4,4,6,6,6,7,8,9)
# mean of the last 3 values
rollmean(a,3) 
rollmean(a,3,fill=NA, align = 'right') # fill with NA's the first 3-1 values


  