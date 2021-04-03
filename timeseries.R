setwd("C:/Users/byjs0/OneDrive/바탕 화면/finalpj")
data = read.csv("u30.csv")
colnames(data)=c('year','month','count')
summary(data)
data$time=seq(1:57)
train = data[1:48,]
test=data[49:57,]
n=dim(train)[1]
library(ggplot2)
ggplot(data=train, aes(x=time, y=count))+geom_point()
x=train$count
x1=ts(x,start=c(2016,1),end=c(2019,12),frequency=12)
plot(x1)
m=decompose(x1,type="multiplicative")                  #seasonal decomposition
season=m$figure                                        #seasonal index
plot(m)

#deseasonalization
for(i in c(1,13,25,37)){
  train$deseason[i]=train$count[i]/season[1]
}
for(i in c(2,14,26,38)){
  train$deseason[i]=train$count[i]/season[2]
}
for(i in c(3,15,27,39)){
  train$deseason[i]=train$count[i]/season[3]
}
for(i in c(4,16,28,40)){
  train$deseason[i]=train$count[i]/season[4]
}
for(i in c(5,17,29,41)){
  train$deseason[i]=train$count[i]/season[5]
}
for(i in c(6,18,30,42)){
  train$deseason[i]=train$count[i]/season[6]
}
for(i in c(7,19,31,43)){
  train$deseason[i]=train$count[i]/season[7]
}
for(i in c(8,20,32,44)){
  train$deseason[i]=train$count[i]/season[8]
}
for(i in c(9,21,33,45)){
  train$deseason[i]=train$count[i]/season[9]
}
for(i in c(10,22,34,46)){
  train$deseason[i]=train$count[i]/season[10]
}
for(i in c(11,23,35,47)){
  train$deseason[i]=train$count[i]/season[11]
}
for(i in c(12,24,36,48)){
  train$deseason[i]=train$count[i]/season[12]
}
out1=lm(deseason~time, data=train)
summary(out1)


#trend prediction w/ deseasonal data
pred1=predict(out1, data=train)

#seasonality back
train$trend=pred1
for(i in c(1,13,25,37)){
  train$forecast[i]=train$trend[i]*season[1]
}
for(i in c(2,14,26,38)){
  train$forecast[i]=train$trend[i]*season[2]
}
for(i in c(3,15,27,39)){
  train$forecast[i]=train$trend[i]*season[3]
}
for(i in c(4,16,28,40)){
  train$forecast[i]=train$trend[i]*season[4]
}
for(i in c(5,17,29,41)){
  train$forecast[i]=train$trend[i]*season[5]
}
for(i in c(6,18,30,42)){
  train$forecast[i]=train$trend[i]*season[6]
}
for(i in c(7,19,31,43)){
  train$forecast[i]=train$trend[i]*season[7]
}
for(i in c(8,20,32,44)){
  train$forecast[i]=train$trend[i]*season[8]
}
for(i in c(9,21,33,45)){
  train$forecast[i]=train$trend[i]*season[9]
}
for(i in c(10,22,34,46)){
  train$forecast[i]=train$trend[i]*season[10]
}
for(i in c(11,23,35,47)){
  train$forecast[i]=train$trend[i]*season[11]
}
for(i in c(12,24,36,48)){
  train$forecast[i]=train$trend[i]*season[12]
}
# In-sample prediction
plot(train$forecast, type="l", xlab="Time", ylab="count", col="blue")
lines(train$count, col="black")
legend(1,530,legend=c("Prediction", "Observation"), col=c("red", "blue"), lty=1, cex=0.8)
for(i in c(1:48)){
  train$yminusyhat[i]=(train$count[i]-train$forecast[i])*(train$count[i]-train$forecast[i])
}
mse_train = mean(train$yminusyhat)
rmse_train=mse_train^(0.5)

# Check heteroscedasticity
library(lmtest)
bptest(out1)

# Out-sample prediction
pred2 = predict(out1, newdata=test)
test$trend = pred2
#row.names(test) = NULL

for (i in c(1:9)){
  test$forecast[i]=test$trend[i]*season[i%%12+1]
}
plot(test$count, type="l", xlab="Time", ylab="count", col="blue")
lines(test$forecast, col="black")
legend(1,690,legend=c("Prediction", "Observation"), col=c("red", "blue"), lty=1, cex=0.8)

for(i in c(1:9)){
  test$yminusyhat[i]=(test$count[i]-test$forecast[i])*(test$count[i]-test$forecast[i])
}
mse_test = mean(test$yminusyhat)
rmse_test=mse_test^(0.5)
########### forecasting #trend prediction w/ de-seasonal data
new1 = data.frame(time=c(49,50,51,52,53,54,55,56,57))
pred2 = predict(out1, newdata=new1)


#seasonality back
forecast2=rep(0,9)
for(i in 1:9){
  forecast2[i]=pred2[i]*season[i]
}
forecast2
library(tseries)
#시계열
d1 = ts(train$count, frequency=12,start=c(2016,1))
plot(d1, main='timeseries')
window(d1,c(2016,1),c(2019,12))
tsp(d1)

plot(d1, main='u30 count')
plot(diff(d1), main='diff u30 count')
#band plot
library(gplots)
library(forecast)
bandplot(train$time, train$count, main='bandplot')
seasonplot(d1, col=rainbow(12), year.labels=T)
ggseasonplot(d1, year.labels=T,continuous=T)
#box-cox 변환
plot(d1, main='boxcox u30 count')#original scale
lambda = BoxCox.lambda(d1)
new = BoxCox(d1, lambda)
plot(new, main='boxcox u30 count')#scle-changed
plot(diff(d1), main='boxcox u30 count')
plot(diff(new), main='diff-boxcox u30 count')
#예측 및 평가
rf=rwf(d1)
plot(rf)
accuracy(rf)
#white noise
plot(d1, xlab='time', main='whitenoise');abline(h=0);
acf(d1)
pacf(d1)
#portmanteau test
Box.test(d1, t=c('B'))
Box.test(d1, t=c('L'))
y = arima.sim
#stationary test(unit-root test)
kpss.test(d1)
######kpss.test(d1,'Trend')
tsdisplay(d1, main='time series display')
#Yule-walker estimation
ar1 = ar(d1, m=c('yule-walker'))
ar1
f1=forecast(ar1)
f1
accuracy(f1)
plot(f1, xlab='time', ylab='series')
abline(h=mean(d1))
grid()
title('\n\nestimation:yule-walker')
Box.test(ar1$resid,type='Ljung-Box')
temp1=window(ar1$resid,start=c(2016,5))
temp1
jarque.bera.test(temp1)
#OLS estimation
ar3=ar(d1,m=c('ols'))
f3=forecast(ar3)
accuracy(f3)
plot(f3, xlab='time', ylab='series')
abline(h=mean(d1))
grid()
title('\n\nestimation:ols')
Box.test(ar3$resid,type='Ljung-Box')
temp3=window(ar3$resid,start=c(2017,3))
temp3
jarque.bera.test(temp3)
#MLE estimation
ar4=ar(d1, m=c('mle'))
f4=forecast(ar4)
accuracy(f4)
plot(f4, xlab='time', ylab='series')
abline(h=mean(d1))
grid()
title('\n\nestimation:mle')
Box.test(ar4$resid,type='Ljung-Box')
temp4=window(ar4$resid,start=c(2016,5))
temp4
jarque.bera.test(temp4)
#최적 lag 선택
library(timsac)
fpeaut(d1)$order
fpeaut(d1)$best.ar
#ARMA
aa=autoarmafit(d1)
ar=aa$model[[1]]$arcoef
ma=aa$model[[1]]$macoef
va=aa$model[[1]]$v
aa
arm11=arma(d1, order=c(1,1))
summary(arm11)
plot(d1,typb='b',main='ARMA(1,1)')

#prdctr(d1, arcoef=ar,macoef=ma,v=va)
#title('\n\nAuto ARMA')

##???
#ARIMA
am1=auto.arima(d1, allowdrift=F)
am1
tsdiag(am1)
fitted(am1)
forecast(am1)
plot(forecast(am1))
lines(fitted(am1),col='red')
plot(d1,main='fitted value')
lines(fitted(forecast(am1),h=1),col=3:2,lty=1)
accuracy(am1)
Box.test(am1$resid,type='Ljung-Box')
Box.test(am1$resid,type='Box-Pierce')

tt=simulate(am1,9)
amtest =test

for(i in c(1:9)){
  amtest$forecast[i] =  tt[i]
}
for(i in c(1:9)){
  amtest$yminusyhat[i] =  (amtest$count[i] - amtest$forecast[i])^2
}
amtest
test
rmse_am1_outsample = (mean(amtest$yminusyhat))^0.5
rmse_am1_outsample

rmse_train=mse_train^(0.5)
rmse_test=mse_test^(0.5)
rmse_train
rmse_test
accuracy(am1)