#ACF and PACF plot to identify appropriate time series model visually 
#Selection of appropriate order of the model based on AIC
#Check evidence of volatility and order of ARCH model graphically
#Selection of appropriate order of the ARCH model based on AIC
install.packages("TSA")
install.packages("forecast")
library(forecast)
install.packages("zoo")
library(zoo)

data<-read.csv(file.choose(), header=T, sep = ";")
head(data)
tail(data)

plot(data$USD.BASED)
acf(log(data$USD.BASED), lag.max = 100, main="ACF log(XU100 Index) USD Based")
#acf slowly decreases(not dies down) hence will need differencing

diffx <- diff(log(data$USD.BASED))
plot(diffx)# looks stationary now

acf(diffx, lag.max = 10, main="ACF of 1st order differencing")
#ACF of first diffrencing of Log indicating signicant value till lag 1 and then ACF cuts approximately

#pacf plot of log series
XX<-log(data$USD.BASED)
pacf(XX, lag.max = 10, main="PACF log(XU100 Index) USD Based")
#PACF of Log XU100 in USD indicating significant value till lag 2 and then PACF cuts.

#PACF plot of single lag difference
XXX<-diff(log(data$USD.BASED))
pacf(XXX, lag.max = 10, main="PACFPACF Difference log(XU100 Index) USD Based")
#PACF of diffrencing Log XU100 in USD, indicating significant value till lag 2 and then PACF cuts approximately.


#Summary of acf and pacf plot:
#ACF slowly decreases (not dies down). This indicates the need of differencing.
#ACF of first order diffrencing of Log XU100 in USD, is indicating significant value till lag 1 and then ACF cuts approximately.
#PACF of Log XU100 in USD, indicating significant value till lag 2 and then PACF cuts.
#PACF of diffrencing Log XU100 in USD, indicating significant value till lag 2 and then PACF cuts approximately.

#Combining all above findings, we can conclude that the tentative model can be assumed as ARIMA(2,1,1),
#but should be verified by AIC based criteria.

#Order of ARIMA Model by AIC
arima(log(data$USD.BASED), order = c(0,1,0))$aic

arima(log(data$USD.BASED), order = c(1, 1, 0))$aic

arima(log(data$USD.BASED), order = c(0, 1, 1))$aic

arima(log(data$USD.BASED), order = c(1, 1, 1))$aic

arima(log(data$USD.BASED), order = c(0, 1, 2))$aic

arima(log(data$USD.BASED), order = c(2, 1, 1))$aic

arima(log(data$USD.BASED), order = c(2, 1, 2))$aic

#As ARIMA(2,1,2)gives the lowest AIC, we choose that as the final model
#and look for evidence of volatility in residuals of ARIMA(2,1,2) model for
#further analysis

#Fitting of ARIMA(2,1,2)
arima212<-arima(log(data$USD.BASED), order = c(2, 1, 2))
arima212

#The model becomes:
#Yt−Yt−1 = −0.495(Yt−1 − Yt−2) − 0.192(Yt−2 − Yt−3)+ 0.660.et−1 + 0.264et−2 + et

#Evidence of volatility: Sq. Residual Plot
res.arima212<-arima212$residuals

sq.res.arima212<-arima212$residuals^2

plot(sq.res.arima212, main="Squared Residuals", ylab="Square residuals of arima(2,1,2)")

#Squared residual plot shows some cluster of volatility at some points of time.

#ACF and PACF of Sq. Residual
par(mfrow=c(1,2))
acf(sq.res.arima212, lag.max = 100, main="ACF Sq residuals")
pacf(sq.res.arima212, lag.max = 30, main="PACF Sq residuals")

#Order of ARCH by AIC
#The order of ARCH components should be selected by AIC values obtained from different choices.
#I am here defining a new R function called ’myAIC()’ for ARCH model
#The function ’garch’ from ’TSA’ package can be used for ARCH, setting order for GARCH components to ’zero’.


install.packages("tseries")
library(tseries)

?garch
arch01 <- garch(res.arima212, order = c(0, 1), trace = F)
N <- arch01$n.used
# ---------user defined code for AIC GARCH model------------
myAIC <- function(fit, q, d) {
N <- fit$n.used
  return(-2 * as.numeric(logLik(fit)) + 2 * (q + 1) * (N - d)/(N - d - q -
                                                                 2))
}
ll = -0.5 * (N - 1) * (1 + log(2 * pi * mean(sq.res.arima212)))
AIC00 <- -2 * ll + 2 * (0 + 1) * (N - 1)/(N - 1 - 0 - 2)
# --Derivation of AIC by the user defined function-------
myAIC(garch(res.arima212, order = c(0, 1), trace = F), 1, 1)


#Order of ARCH by AIC
#order=c(0,2) means garch=0, arch=2
myAIC(garch(res.arima212, order = c(0, 2), trace = F), 1, 1)
#-55885.14

myAIC(garch(res.arima212, order = c(0, 3), trace = F), 1, 1)
#-56061.37

myAIC(garch(res.arima212, order = c(0, 4), trace = F), 1, 1)
##-56568.13

myAIC(garch(res.arima212, order = c(0, 10), trace = F), 1, 1)
##-57055.59

myAIC(garch(res.arima212, order = c(0, 11), trace = F), 1, 1)
##-57053.7

#selected order of model is 10

#Fitting of ARCH(10)
arch10<-garch(res.arima212, order = c(0, 10), trace = F)
summary(arch10)
#box-Ljung test shows errors are iid(white noise)
#H0:model shows no lack of fit/model is fit/errors are iid/no autocorrelation among errors
#H1:model shows lack of fit
#hence no need to do further modelling


