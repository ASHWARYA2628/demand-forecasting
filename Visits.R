#Include the necessary forecast and data manipulation libraries
library(forecast)
library(dplyr)

#Creation of train and test data splits for the model and defining the Time Series Output
mydata<-mydata[1:143,]
mytest<-mydatafull[144:169,]
Visitss = ts(mydata$Visits.TT.Consumer, frequency=52)

#Capping of the data if needed
q95<-quantile(Visitss,0.95)
Visitsx<-ifelse(Visitss>=q95,q95,Visitss)
Visitss<-Visitsx

#Plotting of ACF and PACF graph of the time series
withoutblank<-(complete.cases(Visitss2))
ns <-ndiffs(withoutblank, max.d=2)
tsdisplay(Visitss2)
adf.test(Visitss)

#ADF test for the stationarity of the time series
#Identify lambda parameter for Box Cox transformation so that time series data follows Normal distribution
lambda=BoxCox.lambda(Visitss,lower=0)
lambda

#Regressor Standardization of the budget and other predictor variables to make them unit free
#Regressor Normalization
reg_data<-scale(mydata[,c(18:52,55:74)])
reg_data[is.na(reg_data)]<-0
reg_data<-as.data.frame(reg_data)

reg_test<-scale(mytest[,c(18:52,55:74)])
reg_test[is.na(reg_test)]<-0
reg_test<-as.data.frame(reg_test)

#Visiting the ARIMAX function with desired variables according to significant impact on the time series
fit<-auto.arima(Visitss,xreg=reg_data[reg_list])
predictc<-forecast(fit,xreg=reg_test[reg_list])
predicted<-as.data.frame(predictc)

#MAPE calculation and finding the best fitted model
mapecal<-mape(mytest$`Visits.TT.Consumer`[1:7],predicted$`Point Forecast`[1:7])
mapecal

#ACF and PACF graphs creation of the residuals
#Ljung-Box test for the normality of the residuals

arima_Con_residule <- residuals(fit)
tsdisplay(arima_Con_residule)
Box.test(arima_Con_residule, lag = 16,fitdf= 1 ,type ="Ljung")

#Fetching forecasted values for website visits
final <-predicted$`Point Forecast`
View(final)