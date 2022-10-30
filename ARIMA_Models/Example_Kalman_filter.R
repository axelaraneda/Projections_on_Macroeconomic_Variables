#Install package MARSS and load the library
#install.packages('MARSS')
#library('MARSS')

# We look to fit the following type of models:

# x(t)=u+bx(t-1)+e1(t), e1(t)~N(0,q)    (1)
# y(t)=a+zx(t)+e2(t), e2(t)~N(0,r)      (2)

# where y is the observable variable and x is the state (or hidden) variable
# Eq. 1 and 2 are called Kalman filter, and the Kalman smoother computes the the expected values for x(t)

# In matrix form eq. 1 and 2 are rewriten as:

# X(t)=U+Bx(t-1)+E1(t), E1(t)~MVN(0,Q)    (3)
# Y(t)=A+Zx(t)+E2(t), E2(t)~MVN(0,R)      (4)
# where each capital letter stands for a matrix and MVN is a multivariate standard nornmal distribution

#########################################################

#Example 1
# Using the Nile river flows data (in R is loaded by default using Nile), we want to calibrate the following model:
# x(t)=u+x(t-1)+e1(t), e1(t)~N(0,q)    (5)
# y(t)=x(t)+e2(t), e2(t)~N(0,r)        (6)

# By comparison to Eqs. 1 and 2, we have fixed (a,b,z)=(0,1,1)  

# We proceed using the Kalman smoother MARSS which fit the multivariate models (3,4)
# First, we need to specify the parameters:
mod.list<-list(
  U=matrix('u'),
  x0=matrix('x0'),
  B=matrix(1),
  Q=matrix('q'),
  Z=matrix(1),
  A=matrix(0),
  R=matrix('r'),
  tinitx=0)
# We set the matrices A, B, and Z as one-dimensional matrices (i.e, just a number) with fixed values 0, 1, and 1 respectively.
# We initialize the algorithm at time zero.
# We define matricees U, X_0, Q, and R as one-dimensional matrices of coefficients that shoueld be determined.

fit<-MARSS(Nile,model=mod.list)
plot(Nile) # plot the original data (from 1871 to 1970)
### addig the model fit saved at fit$states[1,]
lines(1871:1970,fit$states[1,], col='red')
# or just automacally plot the data and model estimated including confidence intervals
plot(fit) # press enter to continue or q to quit

# in order see the computed pated parameters
broom::tidy(fit)

# We can forescast also
plot(forecast(fit,10)) # ten years ahead forecasting
#################################

#Example 2
# Using the Nile river flows data (in R is loaded by default using Nile), we want to calibrate the following model:
# x(t)=u+x(t-1)+e1(t), e1(t)~N(0,q)           (7)
# y(t)=x(t)+y(t-1)+e2(t), e2(t)~N(0,r)        (8)
# where 8 is equivalent to
# D(y(t))=x(t)+e2(t), e2(t)~N(0,r)        (9)
# With D=diff()
# There are two ways of how to proceed.
# The first one is to consider eqs 7-9 (i.e, taking differences)

mod.list2.1<-list(
  U=matrix('u'),
  x0=matrix('x0'),
  B=matrix(1),
  Q=matrix('q'),
  Z=matrix(1),
  A=matrix(0),
  R=matrix('r'),
  tinitx=0)
fit2.1<-MARSS(diff(Nile),mod.list2.1)
# The second one is to consider an 'exogeneous' variable equal to the lagged values of Nile (vector d) with coeff equal to 1 (matrix D=1); i.e, eqs 7-8)
mod.list2.2<-list(
  U=matrix('u'),
  x0=matrix('x0'),
  B=matrix(1),
  Q=matrix('q'),
  Z=matrix(1),
  A=matrix(0),
  R=matrix('r'),
  D=matrix(1),
  d=matrix(Nile[1:99],nrow = 1), # lagged values, should have a matrix form
  tinitx=0)
fit2.2<-MARSS(Nile[2:100],mod.list2.2)
# In booth cases we obtain the same parameters
coef(fit2.1)
coef(fit2.2)
broom::tidy(fit2.1)
broom::tidy(fit2.2)

plot(fit2.2)

plot(Nile[2:100],col=2, ylabel=Nile)
values2.2<-fitted(fit2.2,ytT) # Fitted values in the variable level
lines(values2.2$.fitted,col=3)

# Example 2 (Homework 3, excercise 5)
# delta(t)=delta(t-1)+e1                    (7)
# p(t)=alpha+delta(t)*trend(t)+p(t-1)+e2    (8)
# alpha is constant and trend is a vector correspondind to a linear tendency (e.g., 1:T)
# We could rewrite eq. 8 as
# p(t)-p(t-1)=D(p(t))=alpha+delta(t)*trend(t)+e2    (9)
# In this example, the coef z is fixed but varies over time (1:T)
# In this case we need to add a third dimension to the matrix Z
# Creating the trend vector:
L<-length(Nile) #Lenght of data
Trend<-1:(L-1) # from 1 to L-1 (or T-1) we miss one value due to the lag
ZZ<-array(0, dim = c(1, 1,L-1))
ZZ[1,1,]<-Trend


# and now just fix Z as ZZ
mod.list3.1<-list(
  U=matrix(0),
  x0=matrix('x0'),
  B=matrix(1),
  Q=matrix('q'),
  Z=ZZ,
  A=matrix('a'),
  R=matrix('r'),
  tinitx=0)
# and using eq. 9 (i.e., diff(Nile)) the model fit is computed:
fit3.1<-MARSS(diff(Nile),model=mod.list3.1)
plot(fit3.1)
plot(diff(Nile))
fit3.1_values<-fitted(fit3.1)
lines(1872:1970,fit3.1$states, col='red',type='l')
plot(forecast(fit3.1))
#Alternatively

mod.list3.2<-list(
  U=matrix(0),
  x0=matrix('x0'),
  B=matrix(1),
  Q=matrix('q'),
  Z=ZZ,
  A=matrix('a'),
  R=matrix('r'),
  D=matrix(1),
  d=matrix(Nile[1:99],1,99),
  tinitx=0)
fit3.2<-MARSS(Nile[2:100],model=mod.list3.2)
plot(Nile[2:100])
values3.2<-fitted(fit3.2,ytT)
lines(values3.2$.fitted,col='2')
  
