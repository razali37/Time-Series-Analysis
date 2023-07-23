# Importing Libraries 
library(caret)
library(dplyr)
library(e1071)
library(rpart)
library(glue)
library(rpart.plot)
library(class)
library(ggplot2)
library(forecast)
library(urca)
library(tseries)
# Read dataset 
data = read.csv("D:/NAPM.csv")
head(data)
glimpse(data)
# Problem 4
# Time series object dataset
date <- c("1/1/1980", "2/1/1980", "3/1/1980", "4/1/1980", "5/1/1980", "6/1/1980", "7/1/1980")
index <- c(46.2, 50.2, 43.6, 37.4, 29.4, 30.3, 35.0, 45.5, 50.1, 55.5, 58.2, 53.0)
ts_data <- ts(index, start = c(1980, 1), frequency = 12)
# AIC's best fit model using auto.arima
fit <- auto.arima(ts_data, seasonal = FALSE)
summary(fit)
# BIC's best fit model using auto.arima
fit <- auto.arima(ts_data, seasonal = FALSE, ic = "bic")
summary(fit)
# Forecasts with origin at the end of the data
forecast_result <- forecast(fit, h = 5)
# Forecasts and their 95% prediction intervals
print(forecast_result)
# 10-step ahead forecasts
forecast_result <- forecast(fit, h = 10)
plot(forecast_result, main = "10-Step Ahead Forecasts", xlab = "Time", ylab = "Index")
# Data for comparison
lines(ts_data, col = "red")
legend("bottomleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = c(1, 1))
# Analyze  trend consistency
actual_trend <- diff(ts_data, differences = 1)
forecast_trend <- diff(forecast_result$mean, differences = 1)
if (all(sign(actual_trend) == sign(forecast_trend))) {
  message("The forecasts exhibit a trend consistent with the observed dynamic behavior of the process.")
} else {
  message("The forecasts do not exhibit a trend consistent with the observed dynamic behavior of the process.")
}
# f) PMI reading above 50 percent indicates
# forecasted values
forecast_values <- forecast_result$mean

# Determine if the forecasts indicate expansion or contraction
if (all(forecast_values > 50)) {
  message("The model forecasts indicate that the manufacturing economy is generally expanding.")
} else if (all(forecast_values < 50)) {
  message("The model forecasts indicate that the manufacturing economy is generally contracting.")
} else {
  message("The model forecasts do not consistently indicate either expansion or contraction.")
}
# Problem 5
# Read Industrial Production Indexdata 
df =read.csv("D:/indpro.csv")
glimpse(df)
# Time series object for the "rate" variable
date <- c("2/1/1990", "3/1/1990", "4/1/1990", "5/1/1990", "6/1/1990", "7/1/1990", "8/1/1990")
rate <- c(0.0093437280, 0.0053745420, -0.0013140180, 0.0019302970, 0.0032093630, -0.0013140180, 0.0028488280)
ts_rate <- ts(rate, start = c(1990, 2), frequency = 1)
# Data frame for plotting
df <- data.frame(date = as.Date(date, format = "%m/%d/%Y"), rate = ts_rate)
# Time plot
ggplot(df, aes(x = date, y = rate)) +
  geom_line() +
  labs(x = "Date", y = "Index Growth Rate") +
  ggtitle("Time Plot of Index Growth Rate")
# Dickey-Fuller unit root test
df_test <- ur.df(ts_rate, type = "drift")
# Test results
print(summary(df_test))
# KPSS unit root test
kpss_test <- ur.kpss(ts_rate)
# Test results
print(summary(kpss_test))
# Analyze ACF and PACF
acf_result <- acf(ts_rate, lag.max = 10, plot = TRUE)
pacf_result <- pacf(ts_rate, lag.max = 10, plot = TRUE)
# EACF function
eacf <- function(x, lag.max) {
  acf_vals <- acf(x, plot = FALSE)$acf
  pacf_vals <- pacf(x, plot = FALSE)$acf
  eacf_vals <- matrix(0, nrow = lag.max, ncol = lag.max)
  for (p in 1:lag.max) {
    for (q in 1:lag.max) {
      eacf_vals[p, q] <- abs(pacf_vals[p]) + abs(acf_vals[q]) * sqrt(1 - pacf_vals[p]^2)
    }
  }
  return(eacf_vals)
}
# Analyze EACF
eacf_result <- eacf(ts_rate, lag.max = 10)
#EACF result
print(eacf_result)
# Determine stationary behavior
is_stationary <- all(abs(acf_result$acf) <= 2 / sqrt(length(ts_rate)))
# AR, MA, or ARMA behavior
is_AR <- any(eacf_result[1, -1] > eacf_result[1, 1])
is_MA <- any(eacf_result[-1, 1] > eacf_result[1, 1])
is_ARMA <- is_AR && is_MA
# Print the results
cat("Stationary behavior:", ifelse(is_stationary, "Yes", "No"), "\n")
cat("AR behavior:", ifelse(is_AR, "Yes", "No"), "\n")
cat("MA behavior:", ifelse(is_MA, "Yes", "No"), "\n")
cat("ARMA behavior:", ifelse(is_ARMA, "Yes", "No"), "\n")
# auto.arima with BIC criterion
model_M1 <- auto.arima(ts_rate, ic = "bic")
# Check the suggested model order
order_M1 <- model_M1$arma[1:3]
model_M1_fit <- Arima(ts_rate, order = order_M1)
coefficients_M1 <- coef(model_M1_fit)
residuals_M1 <- residuals(model_M1_fit)
acf(residuals_M1, lag.max = 10, main = "ACF of Residuals (M1)")
# Check adequacy of the model
ljung_box_test_M1 <- Box.test(residuals_M1, lag = 20, type = "Ljung-Box")
is_adequate_model_M1 <- ljung_box_test_M1$p.value > 0.05
cat("Order of the ARIMA model (M1):", paste(order_M1, collapse = ","), "\n")
cat("Ljung-Box test p-value (M1):", ljung_box_test_M1$p.value, "\n")
cat("Significance of model coefficients (M1):\n")
print(coefficients_M1)
cat("Adequate model (M1):", ifelse(is_adequate_model_M1, "Yes", "No"), "\n")
model_M2 <- auto.arima(ts_rate, ic = "bic")
order_M2 <- model_M2$arma[1:3]
model_M2_fit <- Arima(ts_rate, order = order_M2)
coefficients_M2 <- coef(model_M2_fit)
# Analyze the residuals
residuals_M2 <- residuals(model_M2_fit)
# ACF of residuals
acf(residuals_M2, lag.max = 10, main = "ACF of Residuals (M2)")
cat("Order of the ARIMA model (M2):", paste(order_M2, collapse = ","), "\n")
cat("Significance of model coefficients (M2):\n")
print(coefficients_M2)
is_adequate_model_M2 <- all(abs(residuals_M2) <= 2 / sqrt(length(ts_rate)))
cat("Adequate model (M2):", ifelse(is_adequate_model_M2, "Yes", "No"), "\n")
forecast_M1 <- forecast(model_M1_fit)
plot(forecast_M1, main = "Forecasts for Model M1")
forecast_M2 <- forecast(model_M2_fit)
plot(forecast_M2, main = "Forecasts for Model M2")
# Compare forecast behavior
par(mfrow = c(1, 2))  # Arrange plots side by side
plot(forecast_M1, main = "Forecasts for Model M1")
plot(forecast_M2, main = "Forecasts for Model M2")
par(mfrow = c(1, 1))  # Reset plotting layout
# Compare forecasts
cat("Forecasts for Model M1:\n")
print(forecast_M1$mean)
cat("\n")
cat("Forecasts for Model M2:\n")
print(forecast_M2$mean)
# Problem 6 
# Read data 
dataset = read.csv("D:/consump.csv")
glimpse(dataset)
time_series <- ts(dataset$pers_inc, start = c(2000, 1), frequency = 12)
# Plotting  time series
ggplot(data = data.frame(date = time(time_series), pers_inc = as.vector(time_series))) +
  geom_line(aes(x = date, y = pers_inc)) +
  labs(x = "Date", y = "pers_inc", title = "Time Plot of pers_inc Variable")
# Plotting ACF and PACF
acf(time_series, lag.max = 20, main = "ACF of pers_inc")
pacf(time_series, lag.max = 20, main = "PACF of pers_inc")
# Plotting EACF
eacf(time_series, lag.max = 20)
#  Dickey-Fuller test
result <- adf.test(time_series)
print(result)
# ARIMA(p, d, q) 
p <- 0  #
q <- 0  
d <- 1 
drift <- FALSE
if (result$p.value >= 0.05) {
  drift <- TRUE
}
# ARIMA model fit
if (drift) {
  model <- Arima(time_series, order = c(p, d, q), include.mean = TRUE)
} else {
  model <- Arima(time_series, order = c(p, d, q), include.mean = FALSE)
}
coefficients <- summary(model)$coef
print(coefficients)
model <- Arima(time_series, order = c(p, d, q), include.mean = drift)
residuals <- residuals(model)
# Plotting residuals over time
ggplot(data = data.frame(date = time(time_series), residuals = residuals)) +
  geom_line(aes(x = date, y = residuals)) +
  labs(x = "Date", y = "Residuals", title = "Residual Analysis")
# Plotting ACF of residuals
acf(residuals, lag.max = 20, main = "ACF of Residuals")
# Plotting histogram of residuals
ggplot(data = data.frame(residuals = residuals)) +
  geom_histogram(aes(x = residuals), bins = 20, fill = "lightblue", color = "black") +
  labs(x = "Residuals", y = "Frequency", title = "Histogram of Residuals")
#  Ljung-Box test
ljung_box_test <- Box.test(residuals, lag = 20, type = "Ljung-Box")
print(ljung_box_test)
# 20-step ahead forecast
forecast <- forecast(model, h = 20)
# 20-step ahead forecast
plot(forecast, main = "20-Step Ahead Forecast")
# ARIMA model
auto_model <- auto.arima(time_series)
# Order of the best model
print(auto_model)
# Ljung-Box test on the previous model
ljung_box_test_manual <- Box.test(residuals(model), lag = 20, type = "Ljung-Box")
print(ljung_box_test_manual)







