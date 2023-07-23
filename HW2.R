############# Problem 1 ############

# load the dataset
groceries <- read.csv("D:/groceries.csv")
# create a time series object for ToothPaste
toothpaste <- ts(groceries$ToothPaste, start = c(2008, 1), frequency = 52)
# plot the lag plot
library(stats)
lag.plot(toothpaste, lag = 1, main = "Lag plot of ToothPaste")
# compute the autocorrelation function
acf(toothpaste, lag.max = 15, main = "Autocorrelation function of ToothPaste")
# add horizontal lines for significance levels
abline(h = c(-1.96/sqrt(length(toothpaste)), 1.96/sqrt(length(toothpaste))), col = "red")
# apply the Ljung Box test up to lag 15
Box.test(toothpaste, lag = 15, type = "Ljung-Box")
# Load the tseries package
library(tseries)
# Perform the ADF test on the ToothPaste series
adf.test(toothpaste)


############# Problem 2 ############



library(stats)

# Load the dataset
intel_data <- read.csv("D:/Intel-1986-2007.csv")
#a
# Compute the log-prices series
log_prices <- log(intel_data$Adj.Price)

# Graph the lag plot of the log-prices series and its first lag
lag.plot(log_prices, lags = 2, main = "Lag plot of log-prices series")
#b
# Compute the ACF of the log-prices series
acf_log_prices <- acf(log_prices, lag.max = 15, plot = TRUE, main = "ACF of log-prices series")
#c
# plot the first 10 lags of the ACF of log prices
acf(log_prices, lag.max=10, main="ACF of Log Prices")
# plot the first 10 lags of the PACF of log prices
pacf(log_prices, lag.max=10, main="PACF of Log Prices")
#d
# Compute the log returns series
log_returns <- diff(log_prices)
# Compute the ACF of the log returns series
acf_log_returns <- acf(log_returns, lag.max = 15, plot = TRUE, main = "ACF of log returns series")
#e
# Compute log returns
log_returns <- diff((log_prices), lag=1)
# Conduct Ljung-Box test on log returns
Box.test(log_returns, lag=15, type="Ljung-Box")

############## Problem 3############################
# Generate AR(1) time series with given parameters
set.seed(123)
n <- 1000
r <- numeric(n)
r[1] <- rnorm(1)
for (i in 2:n) {
  r[i] <- 0.9 * r[i-1] + rnorm(1, mean = 0, sd = sqrt(0.5))
}

# Compute mean and variance of time series
mean_r <- mean(r)
var_r <- var(r)

# Print results
cat("Mean of the time series r:", mean_r, "\n")
if (abs(0.9) < 1) {
  cat("AR(1) model is stationary\n")
} else {
  cat("AR(1) model is non-stationary\n")
}
cat("Overall variance of the time series r:", var_r, "\n")


############## Problem 4########################################


# Generate MA(1) time series with given parameters
set.seed(123)
n <- 1000
x <- numeric(n)
a <- rnorm(n, mean = 0, sd = sqrt(0.025))
x[1] <- 5 + a[1]
for (i in 2:n) {
  x[i] <- 5 + a[i] - 0.5 * a[i-1]
}

# Compute mean and variance of time series
mean_x <- mean(x)
var_x <- var(x)

# Print results
cat("Mean of the time series X:", mean_x, "\n")
cat("Variance of the time series X:", var_x, "\n")
if (abs(-0.5) < 1) {
  cat("MA(1) model is stationary\n")
} else {
  cat("MA(1) model is non-stationary\n")
}




############## Problem 5########################################

# a) Import the data and create a time series object for index using the ts() function where the
# starting date is first month of 1980.
# Load required packages
library(tidyverse)
# Load data
NAPM <- read_csv("D:/NAPM.csv")
# Create time series object
NAPM_ts <- ts(NAPM$index, start = c(1980, 1), frequency = 12)
# b) Create a time plot of the data and determine whether the series is multiplicative or additive.
# Time plot
plot(NAPM_ts, main = "NAPM Index from 1980 to 2015")
# c) Create a decomposition of the series and analyze the series for trends and seasonality.
# Decomposition
NAPM_decomp <- decompose(NAPM_ts)
# Trend component
plot(NAPM_decomp$trend, main = "Trend component of NAPM Index from 1980 to 2015")
# Seasonal component
plot(NAPM_decomp$seasonal, main = "Seasonal component of NAPM Index from 1980 to 2015")
# Random component
plot(NAPM_decomp$random, main = "Random component of NAPM Index from 1980 to 2015")
# d) Analyze if the time series is serially correlated using the ACF plot and the Ljung Box test.
# ACF plot
acf(NAPM_ts)
# Ljung Box test
Box.test(NAPM_ts, lag = 12, type = "Ljung-Box")
# e) Fit an AR(2) model with the Arima function as described in class and write down the
# Fit AR(2) model
NAPM_arima <- arima(NAPM_ts, order = c(2, 0, 0))
# Estimated model
NAPM_arima
#f
library(lmtest)
coeftest(NAPM_arima)


################Problem 6 #####################################################################

# Fit an MA(1) model to the ToothPaste series
ma1_model <- arima(toothpaste, order=c(0,0,1))
# Print the estimated coefficients
summary(ma1_model)
# Test the significance of the coefficients
coeftest(ma1_model)



