#GARCH refers to Generalized AutoRegressive Conditional Heteroscedasticity.
#suitable for high volatile timeseries data.
#Apple Stock prices.

#libraries
install.packages("quantmod")
install.packages(("xts"))
install.packages("PerformanceAnalytics")
install.packages("rugarch")

library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)

#loading data
getSymbols("AAPL", from = "2008-01-01", to = "2020-04-04 ")
chartSeries(AAPL["2019-12"])#candlestick format
chartSeries(AAPL)#Full series
  
#we convert to daily return data
return<-CalculateReturns(AAPL$AAPL.Close)#returns=Pt+1-Pt/Pt


View(return)
#delete first row since it has missing value
return<-return[-1]

#histogram
hist(return)#symmetric distribution
chart.Histogram(return, methods = c('add.density', 'add.normal'), colorset = c('blue', 'green', 'red'))
#blue=histogram, green=density, red=normal curve
#we can see higher green curve than normal and tails are thicker means that we get different what we can expect from normal dist.
#we get very high or very low returns compared to normal expectation. T-dist works better than Normal dist for returns

chartSeries(return, theme = 'white')#stationary time series with mean=0 showing no trend or seastional component
#but lot of variations/volatility. In 2008 financial crisis, hence lot of volatility

#Annulized volatility
sd(return)#1.9%

sqrt(252)*sd(return)  
#31.7%

#volatility for 2008
sqrt(252)*sd(return["2008"])  # 58% in 2008

#volatility for 2019
sqrt(252)*sd(return["2019"]) # 26% in 2019(less than half of 2008)

#ploting monthly rolling for clear visualiation
chart.RollingPerformance(R=return["2008::2019"], width = 22, FUN = "sd.annualized", scale = 252, main = "Apple's Monthly Rolling Volatility")

#ploting yearly rolling for clear visualiation
chart.RollingPerformance(R=return["2008::2019"], width = 252, FUN = "sd.annualized", scale = 252, main = "Apple's Yearly Rolling Volatility")


#standard GARCH model with constant mean

s <-ugarchspec(mean.model = list(armaOrder = c(0,0)),
               variance.model = list(model= "sGARCH"),
               distribution.model ='norm')

m <-ugarchfit(data = return, spec = s)

m 

plot(m, which = 'all')

f <- ugarchforecast(fitORspec = m, n.ahead = 20)

f
                                        
plot(fitted(f))#constant mean 

plot(sigma(f))


#application with portfolio allocation

v = sqrt(252)*sigma(m)#annualized volatility
w = 0.5/v#weights equally assigned

plot(merge(v, w), multi.panel=T)#small weight to high volatility

tail(w)# last weight=0.7433
#hence if amount is 1m to be invested, then we should allocate to risky asset
allocate<-1000000*0.7433519
allocate
#743351 to risky asset
riskfree<-1000000-743351
riskfree#allocate 256649 to riskfree asset(risk is almost zero)

#if we increase weight twice i.e 10% then risk will be double

v = sqrt(252)*sigma(m)#annualized volatility
w = 0.1/v#weights equally assigned

plot(merge(v, w), multi.panel=T)#small weight to high volatility

tail(w)#last weight =0.14 i.e 2 * 0.7

#model-2 i.e GARCH with sstd (skewed student-t distribution)

#copy from above
s <-ugarchspec(mean.model = list(armaOrder = c(0,0)),
               variance.model = list(model= "sGARCH"),
               distribution.model ='sstd')#only change from 'norm' to 'sstd'

m2 <-ugarchfit(data = return, spec = s)

m2#check for Goodness of fit, p-value > 0.05 hence cannot reject H0 and conclude model with sstd is better

plot(m2, which = 'all')# sttd qq plot better than previous and also AIC has decreased also.
#skew is 0.05(<1) meaning we have negative skew distri. 

#model-3 with GJR-GARCH 

s <-ugarchspec(mean.model = list(armaOrder = c(0,0)),
               variance.model = list(model= "gjrGARCH"),
               distribution.model ='sstd')

m <-ugarchfit(data = return, spec = s)

plot(m, which = 'all')

plot(m)# selection 12, plot no.12, we can see news impact is asymmetric i.e positive news has gradual impact but negative news has quick impact

#AIC is further decreased.

#model-4 with AR(1) GJR GARCH

s <-ugarchspec(mean.model = list(armaOrder = c(1,0)),
                variance.model = list(model= "gjrGARCH"),
                distribution.model ='sstd')

#changed armaorder above

m <-ugarchfit(data = return, spec = s)

plot(m, which = 'all')

m#will not use this model because ar1 is not statistically significant in result

#model-5 GJR Garch in Mean.

s <-ugarchspec(mean.model = list(armaOrder = c(0,0), archm = T, archpow = 2), 
                variance.model = list(model= "gjrGARCH"),
                distribution.model ='sstd')

m <-ugarchfit(data = return, spec = s)

m# archm is not significant hence not used.
plot(m5, which = 'all')


#simulation with model-3 i.e. gjrGarch model hence treat this as final model

s <-ugarchspec(mean.model = list(armaOrder = c(0,0)),
               variance.model = list(model= "gjrGARCH"),
               distribution.model ='sstd')

m <-ugarchfit(data = return, spec = s)
sfinal <- s

setfixed(sfinal) <-as.list(coef(m))

#forcast 
f2008 <- ugarchforecast(data=return["/2018-12"], fitORspec = sfinal, n.ahead = 252)

f2019 <- ugarchforecast(data=return["/2019-12"], fitORspec = sfinal, n.ahead = 252)

par(mfrow = c(2,1))
plot(sigma(f2008))
plot(sigma(f2019))

#interpretation of f2008=volatility for next year(252) is likely to decline.
#interpreation of f2019=volatility for next year is likely to increase in 2020, in long-run expected volatility will converge to average.


sim <-ugarchpath(spec = sfinal, m.sim = 3, n.sim = 1*252, rseed = 123)#for year prediction and 3 simulations
plot.zoo(fitted(sim))#returns
plot.zoo(sigma(sim))#volatility.

#actual prices
tail(AAPL)#last closing value was 241.41 
p <-241.41*apply(fitted(sim), 2, 'cumsum') + 241.41
par(mfrow= c(1,1))
matplot(p, type = 'l', lwd=3)
#interpreation: red and green met at 275, red ranges between 275-280, green goes down from 350 to 325
#best black line shows continuous increase and we can expect at the end of 2020 stock price will be 375-380
