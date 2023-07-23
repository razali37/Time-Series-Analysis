# Problem 1
# a)
# Load the required libraries
library(vars)

# Load the dataset
data <- read.csv("groceries (1).csv")

# Convert Date column to Date format
data$Date <- as.Date(data$Date, format = "%d-%b-%y")

# Create a time series object
ts_data <- ts(data[, -1], start = min(data$Date), frequency = 52)

# Plot the autocorrelations for each variable
par(mfrow = c(3, 1))
for (i in 1:3) {
  acf(ts_data[, i], main = colnames(ts_data)[i])
}

# Plot the cross-correlations for each pair of variables
par(mfrow = c(3, 3))
for (i in 1:3) {
  for (j in 1:3) {
    if (i != j) {
      ccf(ts_data[, i], ts_data[, j], main = paste(colnames(ts_data)[i], colnames(ts_data)[j], sep = " vs. "))
    }
  }
}

# b)
# Determine the sensible maximum lag for the VAR model using VARselect
var_select <- VARselect(ts_data, lag.max = 10, type = "both")
sensible_lag <- var_select$selection[1]

# Fit the VAR model with the selected lag
var_model <- VAR(ts_data, p = sensible_lag)

# c)
# Test the goodness of fit
var_diag <- serial.test(var_model, lags.pt = sensible_lag, type = "PT.asymptotic")

# d)
# Forecast 15 steps ahead
var_forecast <- predict(var_model, n.ahead = 15)

# Analyze forecast performance for each variable
par(mfrow = c(3, 1))
for (i in 1:3) {
  plot(ts_data[, i], xlim = c(max(data$Date), max(data$Date) + 15), ylim = range(ts_data[, i], var_forecast$fcst[[i]][, 1]))
  lines(var_forecast$fcst[[i]][, 1], col = "red")
  legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "red"), lty = 1)
}




# Problem 2

library(fpp2)
library(ggplot2)
library(forecast)

# Create a data frame with the provided column names
dataset <- data.frame(Date = time(gasoline), Value = gasoline)

# a) Plot the series to check for trends
ggplot(dataset, aes(x = Date, y = Value)) +
  geom_line() +
  labs(x = "Date", y = "Gasoline Supply (millions of barrels per day)") +
  theme_minimal()

# b) Fit a loess line to the data
loess_fit <- loess(Value ~ Date, data = dataset)
dataset$loess_fit <- predict(loess_fit)

ggplot(dataset, aes(x = Date, y = Value)) +
  geom_line() +
  geom_line(aes(y = loess_fit), color = "blue") +
  labs(x = "Date", y = "Gasoline Supply (millions of barrels per day)") +
  theme_minimal()

# c) Extract the residuals from the loess fit
dataset$residuals <- residuals(loess_fit)
residuals_ts <- ts(dataset$residuals, start = start(dataset$Date), frequency = frequency(dataset$Date))

# d) Plot and analyze the spectral density of the residual series
spec <- spectrum(residuals_ts)
plot(spec, main = "Spectral Density of Residuals")

# e) Model the time series with harmonic regression using tbats
harmonic_model <- tbats(residuals_ts)
harmonic_model
# f) Analyze the model for goodness of fit and run a two-year forecast
harmonic_fit <- forecast(harmonic_model, h = 104)
harmonic_fit
plot(harmonic_fit, main = "Harmonic Regression Forecast")
