mydata<-read.table(url("https://raw.githubusercontent.com/macrandrea/ATSA/master/GDP%20ITA%20SA%20EURO-%20working%20day%20and%20seasonally%20adjusted.txt"), header=TRUE,dec=".")
#we choose the attached text file: "GDP ITA SA EURO- working day and seasonally adjusted"
attach(mydata)
names(mydata)
mydata
gdp_ts<-ts(gdp,frequency=4, start=c(1995,1)) 
windows()
ts.plot(gdp_ts,ylim=c(350000.0, 430000.0))
#there is a trend
library(CADFtest)
dim(mydata)
max.lag=round(sqrt(99))
CADFtest(gdp_ts,type="trend",criterion="BIC", max.lag.y=max.lag)
#stochastic trend
dgdp_ts=diff(gdp_ts)
windows()
ts.plot(dgdp_ts)
CADFtest(dgdp_ts,type="drift",criterion="BIC", max.lag.y=max.lag)
#no trend and is stationary
windows()
acf(dgdp_ts) #MA(2)
pacf(dgdp_ts) #AR(1)
#############AR##########
fit_ar<-arima(gdp_ts, order=c(1,1,0),seasonal=c(0,0,0))
fit_ar
abs(0.5729/0.0816) #>2 i.e. significant
acf(fit_ar$res)
Box.test(fit_ar$res, lag=max.lag, type="Ljung-Box")
#they are white noise but a bit borderline, we go for the MA
#############MA###########
fit_ma<-arima(gdp_ts, order=c(0,1,2),seasonal=c(0,0,0))
fit_ma
abs(0.4418/0.0882)#>2 i.e. significant
abs(0.4438/0.0954)#>2 i.e. significant
acf(fit_ma$res)
Box.test(fit_ma$res, lag=max.lag, type="Ljung-Box")
#white noise
############ARIMA#########
#we give a try for an arima, just in case
fit_arima<-arima(gdp_ts, order=c(1,1,2),seasonal=c(0,0,0))
fit_arima
abs(0.3415/ 0.1934)
abs(0.1765/0.1793)
abs(0.3370/0.1394)
#coefficients are not significant, we keep the MA
#################################
#forecasrting####################
#################################
#MA 
myforecastMA<-predict(fit_ma,n.ahead=10)
expected<-myforecastMA$pred
lower<-myforecastMA$pred-qnorm(0.975)*myforecastMA$se
upper<-myforecastMA$pred+qnorm(0.975)*myforecastMA$se
cbind(lower,expected,upper)
windows()
plot.ts(gdp_ts,xlim=c(2017,2022),ylim=c(350000.0, 430000.0))
lines(expected,col="red")
lines(lower,col="blue")
lines(upper,col="blue")
#AR
myforecastAR<-predict(fit_ar,n.ahead=10)
expected<-myforecastAR$pred
lower<-myforecastAR$pred-qnorm(0.975)*myforecastAR$se
upper<-myforecastAR$pred+qnorm(0.975)*myforecastAR$se
cbind(lower,expected,upper)
windows()
plot.ts(gdp_ts,xlim=c(2017,2022),ylim=c(350000.0, 430000.0))
lines(expected,col="red")
lines(lower,col="blue")
lines(upper,col="blue")
############################
#MA gives a better forecast that is near the last observed value and it's better validated
#AR is more parsimonious
#in any case they have similar values of AIC and BIC, they are more or less similar on forecasting
#we could try a diebold mariano
#generate out-of-sample forecast errors
#MA MAE
y<-gdp_ts
S=round(0.75*length(y))
h=1
error1.h<-c()
for (i in S:(length(y)-h))
{
mymodel.sub<-arima(y[1:i], order = c(0,1,2),seasonal=c(0,0,0))
predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
error1.h<-c(error1.h,y[i+h]-predict.h)
}
#AR MAE
error2.h<-c()
for (i in S:(length(y)-h))
{
mymodel.sub<-arima(y[1:i], order = c(3,1,1),seasonal=c(0,0,0))
predict.h<-predict(mymodel.sub,n.ahead=h)$pred[h]
error2.h<-c(error2.h,y[i+h]-predict.h)
}
cbind(error1.h,error2.h)
MAE1<-mean(abs(error1.h))
MAE2<-mean(abs(error2.h))
library(forecast)
dm.test(error1.h,error2.h,h=h,power=1)
#################################
AIC(fit_ar)
AIC(fit_arima)
AIC(fit_ma)
AIC(fit_ar,k=log(64))
AIC(fit_arima,k=log(64))
AIC(fit_ma,k=log(99))
#we definitely choose the MA at this point but we do not drop the AR
#not significally different, but in any case the MA is chosen since it has better characteristics
#########################DISTRIBUTED LAG MODEL################################
#CAN WE USE GER GDP TO PREDICT ITA GDP?
mydata2<-read.table(url("https://raw.githubusercontent.com/macrandrea/ATSA/master/GDP%20GER%20SA%20EURO%20-workingday%20and%20seasonally%20adjusted.txt "), header=TRUE,dec=".")
#we choose the attached text file: "GDP GER SA EURO -workingday and seasonally adjusted"
attach(mydata2)
names(mydata2)
mydata2
gdpg_ts<-ts(gdp,frequency=4,start=c(1995,1))
gdpg_ts
ts.plot(gdpg_ts)
windows()
ts.plot(gdpg_ts,gdp_ts,col=c("red","black"))
#cointegration probably
CADFtest(gdpg_ts,type="trend",criterion="BIC",max.lag.y=max.lag)
#nonstationary
dlgdpg_ts<-diff(log(gdpg_ts))
windows()
ts.plot(dlgdpg_ts)
CADFtest(dlgdpg_ts,type="drift",criterion="BIC",max.lag.y=max.lag)
#stationary
dlgdp_ts<-diff(log(gdp_ts))
windows()
ts.plot(dlgdp_ts)
CADFtest(dlgdp_ts,type="drift",criterion="BIC",max.lag.y=max.lag)
#stvationary
#both integrated of order 1
#DISTRIBUTED LAG MODEL 1#
fit_dlm1=lm(dlgdp_ts~dlgdpg_ts)
fit_dlm1
summary(fit_dlm1)
acf(fit_dlm1$residuals)
Box.test(fit_dlm1$residuals, lag = max.lag, type = "Ljung-Box")
#errors are not white noise from the graph and aren't white noise from the test
#one's not to rely on the model so we go for more lags
#DISTRIBUTED LAG MODEL 2#
lag<-2
n <- length(dlgdp_ts)
dlgdp.0 <- dlgdp_ts[(lag+1):n]
dlgdpg.0 <- dlgdpg_ts[(lag+1):n]
dlgdpg.1 <- dlgdpg_ts[lag:(n-1)]
dlgdpg.2 <- dlgdpg_ts[(lag-1):(n-2)]
fit_dlm2<-lm(dlgdp.0~dlgdpg.0+dlgdpg.1+dlgdpg.2)
fit_dlm2
summary(fit_dlm2)
windows()
acf(fit_dlm2$residuals)
Box.test(fit_dlm2$residuals, lag = max.lag, type = "Ljung-Box")
#DISTRIBUTED LAG MODEL 4#
lag <- 4
n <- length(dlgdp_ts)
dlgdp.0 <- dlgdp_ts[(lag+1):n]
dlgdpg.0 <- dlgdpg_ts[(lag+1):n]
dlgdpg.1 <- dlgdpg_ts[lag:(n-1)]
dlgdpg.2 <- dlgdpg_ts[(lag-1):(n-2)]
dlgdpg.3 <- dlgdpg_ts[(lag-2):(n-3)]
dlgdpg.4 <- dlgdpg_ts[(lag-3):(n-4)]
fit_dlm4<-lm(dlgdp.0~dlgdpg.0+dlgdpg.1+dlgdpg.2+dlgdpg.3+dlgdpg.4)
fit_dlm4
summary(fit_dlm4)
windows()
acf(fit_dlm4$residuals)
Box.test(fit_dlm4$residuals, lag = max.lag, type = "Ljung-Box")
#DISTRIBUTED LAG MODEL 5#
lag <- 5
n <- length(dlgdp_ts)
dlgdp.0 <- dlgdp_ts[(lag+1):n]
dlgdpg.0 <- dlgdpg_ts[(lag+1):n]
dlgdpg.1 <- dlgdpg_ts[lag:(n-1)]
dlgdpg.2 <- dlgdpg_ts[(lag-1):(n-2)]
dlgdpg.3 <- dlgdpg_ts[(lag-2):(n-3)]
dlgdpg.4 <- dlgdpg_ts[(lag-3):(n-4)]
dlgdpg.5 <- dlgdpg_ts[(lag-4):(n-5)]

fit_dlm5<-lm(dlgdp.0~dlgdpg.0+dlgdpg.1+dlgdpg.2+dlgdpg.3+dlgdpg.4+dlgdpg.5)
fit_dlm5
summary(fit_dlm5)
windows()
acf(fit_dlm5$residuals)
Box.test(fit_dlm5$residuals, lag = max.lag, type = "Ljung-Box")
#NO DISTRIBUTED LAG MODEL APPLICABLE
#WE TRY WITH AN AUTOREGRESSIVE DISTRIBUTED LAG MODEL#
#ADLM(3)#
lag <- 3
dlgdp.0 <- dlgdp_ts[(lag+1):n]
dlgdpg.0 <- dlgdpg_ts[(lag+1):n]
dlgdp.1 <- dlgdp_ts[lag:(n-1)]
dlgdpg.1 <- dlgdpg_ts[lag:(n-1)]
dlgdp.2 <- dlgdp_ts[(lag-1):(n-2)]
dlgdpg.2 <- dlgdpg_ts[(lag-1):(n-2)]
dlgdp.3 <- dlgdp_ts[(lag-2):(n-3)]
dlgdpg.3 <- dlgdpg_ts[(lag-2):(n-3)]
fit_adlm3<-lm(dlgdp.0~dlgdpg.0+dlgdp.1+dlgdpg.1+dlgdp.2+dlgdpg.2+dlgdp.3+dlgdpg.3)
fit_adlm3
summary(fit_adlm3)
windows()
acf(fit_adlm3$residuals)
Box.test(fit_adlm3$residuals, lag = max.lag, type = "Ljung-Box")
#good model
#Rsq explains 50%of the percentage increase of gdp
#Fstat pval tells us that together pmts are significant even if the first
#two are the significant ones under the t-test
#############################GRANGER CAUSALITY#############################
fit_adlm_nox <- lm(dlgdp.0 ~ dlgdp.1+dlgdp.2+dlgdp.3)
anova(fit_adlm3,fit_adlm_nox)
#pval is small so gdp germany has incremental explanatory power on gdp italy 
#WE DO REJECT H0 OF NO GRANGER CAUSALITY
fit_ci <- lm(gdp_ts ~ gdpg_ts)
res_fit_ci <- fit_ci$residuals
CADFtest(res_fit_ci,type="drift",criterion="BIC",max.lag.y=max.lag)
#NO COINTEGRATION
##########################VECTOR AUTOREGRESSIVE ANALYSIS##################################
dlogdata<-data.frame(dlgdp_ts,dlgdpg_ts)
names(dlogdata)<-c("dlogITA","dlogGER")
attach(dlogdata)
library(vars)
fit_var1<-VAR(dlogdata,type="const",p=1)
summary(fit_var1)
var1_residuals<-resid(fit_var1)
par(mfrow=c(2,2))
acf(var1_residuals[,1])
acf(var1_residuals[,2])
ccf(var1_residuals[,1],var1_residuals[,2])
#the model is not valid, we still have some autocorr, thus is not a multivar
#white noise, let's see what the autoselect has to say
VARselect(dlogdata,lag.max=10,type="const")
fit_varautom<-VAR(dlogdata,type="const",p=9) 
summary(fit_varautom)
varautom_residuals<-resid(fit_varautom)
#win.graph()
par(mfrow=c(2,2))
acf(varautom_residuals[,1])
acf(varautom_residuals[,2])
ccf(varautom_residuals[,1],varautom_residuals[,2])
#the model has a controverting validity since two lags, distant in time are
#a little tiny bit over the confidence interval
#we decide to ignore them and we keep the VAR(9) even if it's less parsimonous
#but fits good
##########################IMPULSE RESPONSE FUNCTIONS######################à
irf_var<-irf(fit_varautom,ortho=F,boot=T)
win.graph()
plot(irf_var)
predict(fit_varautom, n.ahead=4)

