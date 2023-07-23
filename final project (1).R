library(stats)
library(zoo)
library(ggplot2)
library(ggfortify)
library(forecast)
library(lmtest)
library(fpp2)
library(fUnitRoots)
library(tseries)
library(TSA)   
source("backtest.R")


#### 1. Plot the time series data vs time ####
wti<- read.csv("wti-weekly.csv")
head(wti)
wti$Date = as.Date(wti$Date, format = "%Y-%m-%d")
head(wti)
tail(wti)

#check for missing values
sum(is.na(wti))

wits = ts(wti$Price, start=c(1986, 1), frequency=52)
wits

#check for outliers
outliers_wits<-tsoutliers(wits)
outliers_wits

tsclean(wits)-> new_data

autoplot(wits)
autoplot(new_data)


#Decompose 
dec = decompose(wits, type = "multiplicative")
autoplot(dec)

dec1 <- decompose(new_data, type = "multiplicative")
autoplot(dec1)

# to make the variance equal we use log
q<-log(wits)
autoplot(q)

#This often helps in transforming the data to achieve stationarity,
#noting that taking the logarithm and differencing are common techniques used 
#to stabilize the variance and remove trends in a time series to make it more stationary. 

# to make the mean constant we use diff 
s<-diff(log(wits))
autoplot(s)

# Q IS THE DATA WITH LOG TRANSFORMATION

# S IS THE DATA WITH THE LOG RETURNS TRANSFORMATION


#plot the data
autoplot(wits)
autoplot(q)
autoplot(s)


# Plot the ACF, PACF and EACF function on the log returns data with (20 lags)

Acf(s, lag.max = 20, plot = T) # q value would be the 1st line that comes before the inverted line.
pacf(s, lag.max = 20, plot = T) # p value would be the 0th line that comes before the inverted line.
eacf(s)

Acf(diff(log(new_data)),lag.max = 20, plot = T)
pacf(diff(log(new_data)))

# Jarque-Bera test
jarque.bera.test(s)  #p-value<0.05 -> reject H0 -> not normal distribution

#In the result of the above test, the p-value is less than 0.05, which is a common significance level. 
#Therefore, based on the test result, you would 
#reject the null hypothesis and conclude that 
#the data represented by q does not follow a normal distribution.

# Dickey-Fuller Test & KPSS Test on series "q" -(log(wits))
t.test(q)
adfTest(q, type="nc") # output suggests that the series is non stationary with the drift

# Tests on series "s" - diff(log(wits))
t.test(s)
adfTest(s, type="nc") # the series does not show any trends

#kpss test
kpss.test(q, null="Level") # non stationary series as p value(0.01) is less than threshold (0.05)

kpss.test(s, null="Level")  # stationary series as p(0.1) value is greater than threshold (0.05)
# based on the KPSS test result 
# the p-value (0.1) is greater than the commonly used significance level of 0.05, 
# there is not enough evidence to reject the null hypothesis of level stationarity. 
# This suggests that the series represented by s is consistent with level stationarity, 
# indicating a constant mean over time.
# Therefore, in this case, it can be concluded that the series s is stationary 
# or exhibits stationarity around a constant mean.

kpss.test(diff(log(new_data)), null="Level")
########### MODEL SELECTION ################

acf(s)
eacf(s)

fit1 = Arima(diff(log(wits)), order=c(3, 1, 2))  # this model performs better
fit1
coeftest(fit1)

fit1_drift <- Arima(diff(log(wits)), order=c(3, 1, 2), include.drift=TRUE)
fit1_drift
plot(residuals(fit1_drift), ylab="Residuals", main="Residual Plot - Model with Drift")


fit1_seasonal <- Arima(diff(log(wits)), order=c(1, 1, 2), seasonal=list(order=c(1, 1, 2), period=12))
fit1_seasonal
plot(residuals(fit1_seasonal), ylab="Residuals", main="Residual Plot - Model with Seasonality")
# the model shows the there is a presence of seasonality which repeats every 12 months

# Model with Cleaned Data

fit2 = Arima(diff(log(new_data)), order=c(1, 1, 2)) 
fit2
plot(residuals(fit2), ylab="Fit2_Residuals", main="Residual Plot - Model with cleaned data")
coeftest(fit2)

fit2_seasonal <- Arima(diff(log(new_data)), order=c(1, 1, 2), seasonal=list(order=c(1, 1, 2), period=12))
fit2_seasonal
plot(residuals(fit2_seasonal), ylab="Residuals", main="Residual Plot - Model with Seasonality")
coeftest(fit2_seasonal)

fit2_drift <- Arima(diff(log(new_data)), order=c(2, 1, 2), include.drift=TRUE)
fit2_drift
coeftest(fit2_drift)
plot(residuals(fit2_drift), ylab="Residuals", main="Residual Plot - Model with Drift")
# the above model does not show any drift or trend in the series.

fitauto<- auto.arima(diff(log(new_data)))
summary(fitauto)
#### Model Estimation and Diagnostic ####

# Plot ACF of residuals
acf(fit1_drift$residuals) 
acf(fit1_seasonal$residuals) 
acf(fit1$residuals)

acf(fit2$residuals)
acf(fit2_seasonal$residuals) 
acf(fit2_drift$residuals)

# Ljung box test
Box.test(fit1_drift$residuals, lag=7, type="Ljung")
Box.test(fit1_seasonal$residuals, lag=7, type="Ljung")

Box.test(fit1$residuals, lag=7, type="Ljung")

#the Box-Ljung test performed on the residuals of the "fit1" model, 
#the test yields a chi-squared statistic of 12.279 with 7 degrees of freedom and 
#a p-value of 0.09174. Since the p-value is greater than the chosen significance level (e.g., 0.05), 
#we do not have sufficient evidence to reject the null hypothesis of no autocorrelation in the residuals. 
#This suggests that there is no significant autocorrelation present in the residuals.

Box.test(fit2$residuals, lag=7, type="Ljung")

#In this case, the Ljung-Box test results in a test statistic of 6.4394 with 7 degrees of freedom 
#and a corresponding p-value of 0.4895. Since the p-value is greater than the 
#conventional significance level of 0.05, we fail to reject the null hypothesis of no autocorrelation 
#in the residuals. This suggests that the residuals from fit2 
#do not exhibit significant autocorrelation up to a lag of 7.

#Overall, based on the coefficient estimates, their significance, 
#and the Ljung-Box test results, fit2 appears to be a reasonably good model 
#for the differenced and log-transformed time series with outlier treatment.

Box.test(fit2_seasonal$residuals, lag=7, type="Ljung")

#The null hypothesis of the Box-Ljung test is that there is no autocorrelation in the residuals.
#If the p-value is greater than the chosen significance level (commonly 0.05), 
#we fail to reject the null hypothesis and conclude that 
#there is no significant evidence of autocorrelation in the residuals.

Box.test(fit2_drift$residuals, lag=7, type="Ljung")


Box.test(fitauto$residuals, lag=7, type="Ljung")

#### Predictions #### 
# Forecast next 10 years (10*12 months)
pred_time <- forecast(fit1, h = 10*12)
plot(pred_time)

# View the predicted values
pred_time$mean
autoplot(pred_time)

#### Forecast #### 
fc1 = forecast(fit1_drift, h=28)
fc1
plot(fc1) 

#### Forecast #### 
fc2 = forecast(fit1_seasonal, h=28)
fc2
plot(fc2) 

#### Forecast #####
fc3 <- forecast(fit2, h=10*12)
fc3
plot(fc3)

fc4 <- forecast(fit2_seasonal, h=10*12)
fc4
plot(fc4)

fc5 <- forecast(fitauto, h=10*12)
fc5
plot(fc5)

############## GARCH ############

# Install and load the required package
install.packages("rugarch")
library(rugarch)

# Prepare the data (assuming 'new_data' is the time series data)
returns <- diff(log(new_data))

# Specify the GARCH model
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0)),
                   distribution.model = "std")

# Fit the GARCH model
garch_model <- ugarchfit(spec, data = returns)

# Print the summary of the GARCH model
summary(garch_model)

# Forecast volatility
forecast <- ugarchforecast(garch_model, n.ahead = 10)

# Extract forecasted volatility
volatility <- sigma(forecast)

# View the forecasted volatility
print(volatility)

# Plotting the GARCH model
# We have already fitted the GARCH model and obtained the forecasted volatility
# Plotting the forecasted volatility

plot(volatility, type = "l", xlab = "Time", ylab = "Volatility", main = "GARCH Model - Forecasted Volatility")

