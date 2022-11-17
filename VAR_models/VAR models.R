###### Topic IV.- Vector Autoregressive  (VAR) Models ###############################################
##############################################################################################


rm(list=ls())
graphics.off()
#dev.off() # reset graphic settings

#install.packages("vars")
library("vars")


# Example 1 Canadian economy
data("Canada")
summary(Canada)
plot(Canada, nc = 2, xlab = "")
plot(Canada, xlab = "")

##############################################################################################
# raiz unitaria

adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1
adf2 <- summary(ur.df(Canada[, "prod"], type = "drift", lags = 1))
adf2

#or Equivalently

library('tseries')
tseries::adf.test(Canada[, "prod"],k=1)

##############################################################################################
# VAR estimation

VARselect(Canada, lag.max = 8, type = "both") #selecting optimal lag based on some criteria
#type='trend' : Including trend (We should include it if we have trendy data)
#type='const' : Include a constant term (drift)
#type='both' : Include trend and drift
#type='none': trend and drift are not included

Canada <- Canada[, c("prod", "e", "U", "rw")]
p1ct <- VAR(Canada, p = 1, type = "both") # VAR model of 1-lag including trend and constant
p1ct

summary(p1ct, equation = "e") 
plot(p1ct, names = "e") # graphics for the variable e: model, data, and residuals

#diagnosis test
#serial correlation
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial
#normality
norm1 <- normality.test(p1ct)
norm1$jb.mul
#stability (structural breaks)
stab<-stability(p1ct)
par(mar = c(2, 2, 1, 1))
plot(stab)

##############################################################################################

# Impulse response Analysis

var.irf <- irf(p1ct, response = "U", n.ahead = 80, boot = TRUE) #80 days ahead response in the variable U including bootstraping error bands
var.irf2 <- irf(p1ct,impulse='e', response = "U", n.ahead = 80, boot = TRUE,runs=500)# computes the response in U given a shock in e

print(var.irf)
plot(var.irf2)


oir <- irf(p1ct, impulse = "U", n.ahead = 8, ortho = TRUE, runs = 1000, seed = 12345) # compute the response in all the variables given a shock in U. A seed fix the bootstrap random generator.
plot(oir)

#variance decomposition
fevd(p1ct, n.ahead = 48)
fevd.U <- fevd(p1ct, n.ahead = 48)$U
summary(fevd.U)
plot(fevd.U)
plot(fevd.U[,1])
##############################################################################################
# Test for Granger Causality
var.cg <- VAR(Canada, p = 2, type = "const") # Var(2) including constant term
summary(var.cg)
CAUS<-causality(var.cg, cause = "e") # Granger Causality test (e Granger-cause all the other time-series)
print(CAUS$Granger)
?causality
print(causality(VAR(Canada, p = 4, type = "const"), cause = "e")$Granger)

##############################################################################################
for (i in 1:4)
{
  cat("LAG =", i)
  print(causality(VAR(Canada, p = i, type = "const"), cause = "prod")$Granger)
}
#testing Granger-causality under different lags

##############################################################################################

# Forecasting
forecast <- predict(var.cg , n.ahead = 12, ci = 0.95)
forecast
par(mar = c(1.75, 2, 1, 1))        # Changing area margins
plot(forecast)
##############################################################################################

# Okun's Law  

library(readr)
Okun <- read_csv("~/Desktop/Teaching/Macro/Okun.csv")
U<-Okun$U
G<-Okun$G

U<-ts(U, start=c(1948,1), frequency=4)
G<-ts(G, start=c(1948,1), frequency=4)
data<-data.frame(U,G)

tseries::adf.test(U,k=1)
tseries::adf.test(G,k=1)
#both are stationary




VARselect(data, lag.max = 8, type = "none") # 2 or 8 lags
varmodel<-VAR(data,p=2,type='const') # we discard the trend because the time-series are stationary
summary(varmodel)
############
dev.off()
v<-stability(varmodel)
plot(v$stability$U)
plot(v$stability$G)
plot(stability(varmodel)) # model is stable
test_serial<-serial.test(varmodel)
test_serial # Output indicates non-serial correlation
norm_test<-normality.test(varmodel)
norm_test #non-normal
#########

IRF1<-irf(varmodel, impulse="G",response = "U", n.ahead = 40, boot = TRUE)
plot(IRF1)
# A shock in GDP decrease the unemployement rate (Okun's law)
IRF2<-irf(varmodel, impulse="U",response = "G", n.ahead = 40, boot = TRUE)
plot(IRF2)

causality(varmodel, cause = "G")$Granger
#G Granger-cause U
causality(varmodel, cause = "U")$Granger
#U Granger-cause G

forecast <- predict(varmodel , n.ahead = 10, ci = 0.95)
par(mar=c(3,2,1,0.5))
plot(forecast)
plot(forecast, xlim=c(280,310))
###############

data(PhillipsCurve)
p<-PhillipsCurve[,1] # Logarithm of the consumer price index
w<-PhillipsCurve[,2] # Logarithm of nominal wages
u<-PhillipsCurve[,3] # Unemployement rate
par(mfrow=c(3,1))
plot(p)
plot(w)
plot(u)
Phillips<-data.frame(p,w,u)
VARselect(Phillips, lag.max = 8, type = "both") # 2 lags
var2<-VAR(Phillips,p=2,type='both')
summary(var2)
# we have several non-significant coefficients
# we could fix to zero the non-significant ones by two ways.
# The first one is manually. We can set the zero-value the the desired coefs.
c1<-c(1,0,0,1,0,0,0,0) # coefs. for variable p
c2<-c(1,1,1,1,0,1,0,0) # coefs. for variable w
c3<-c(0,0,1,0,0,0,0,0) # coefs. for variable u
C<-rbind(c1,c2,c3)
#each row relates one equation and each column the coefficient of that eq. 
# 0 fix coef. to zero and 1 indicates that it will be estimated.
# useful to fix to zero a couple of coefficients
v1<-restrict(var2,method = "manual", resmat = C)
summary(v1)

# The other one is just fix at zero all the coefficients with t-statistic below some threshold (p-value>0.05)
# in our case, given that our model has 121 DF (degree of freedoms), a pvalue<0.05 means t>1.98.
# usually we just need to fix t>2 and we are covered in any case.
v2<-restrict(var2,method = "ser", thresh = 1.98)
summary(v2)
plot(irf(v2,impulse='p'))
plot(irf(v2,impulse='w'))
plot(irf(v2,impulse='u'))
forecast2 <- predict(v2 , n.ahead = 10, ci = 0.95)
par(mar=c(2,2,1,0.5))
plot(forecast2)
plot(forecast2, xlim=c(125,141))
