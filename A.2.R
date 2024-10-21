













































# Load necessary libraries
install.packages('forecast')
library(forecast)
install.packages("tseries")
library(tseries)


####Q-1) for AR(1) MODEL Xt = 0.5Xt-1+et
# Set seed for reproducibility
set.seed(123)

#1)
#1.a Simulate 100 observations from AR(1) process
n <- 100
phi <- 0.5
epsilon <- rnorm(n, mean = 0, sd = 1)
X <- numeric(n)
X[1] <- epsilon[1]  # Assume starting point X0 is epsilon[1]

# Generate the AR(1) series
for (t in 2:n) {
  X[t] <- phi * X[t-1] + epsilon[t]
}

#1.b) Plot the time series
plot.ts(X, main = "Simulated AR(1) Process", ylab = "X_t", col = "blue")


#2)
#2.i) Plot ACF and PACF
par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid
acf(X, main = "ACF of AR(1) Process")
pacf(X, main = "PACF of AR(1) Process")
#par(mfrow = c(1, 1))  # Reset plotting grid

#3)
#3.i) Fit AR(1), AR(2) model
model_ar1 <- arima(X, order = c(1, 0, 0))  # AR(1) model
summary(model_ar1)
model_ar1 <- arima(X, order = c(2, 0, 0))  # AR(1) model
summary(model_ar1)

#3.ii) Forecast the next 10 observations
forecast_ar1 <- forecast(model_ar1, h = 10)
forecast_ar1
# Plot the forecast
plot(forecast_ar1, main = "Forecast from AR(1) Model")


#1.c) Estimate the AR(1) parameter
phi_est <- coef(model_ar1)["ar1"]
cat("Estimated AR(1) parameter (ϕ):", phi_est, "\n")




####Q-2) for MA(1) Xt = et + 0.5et-1
# Load necessary libraries
install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)


# Set seed for reproducibility
set.seed(123)

#1.a)
# Simulate 100 observations from MA(1) process
n <- 100
theta <- 0.5
epsilon <- rnorm(n, mean = 0, sd = 1)
X <- numeric(n)
X[1] <- epsilon[1]

# Generate the MA(1) series
for (t in 2:n) {
  X[t] <- epsilon[t] + theta * epsilon[t-1]
}

#1.b) Plot the time series
plot.ts(X, main = "Simulated MA(1) Process", ylab = "X_t", col = "blue")




#2)
#2.1) Plot ACF and PACF
par(mfrow = c(1, 2))
acf(X, main = "ACF of MA(1) Process")
pacf(X, main = "PACF of MA(1) Process")


#3)
#3.i) Fit MA(1), MA(2) model
model_ma1 <- arima(X, order = c(0, 0, 1))  # MA(1) model
summary(model_ma1)
model_ma1 <- arima(X, order = c(0, 0, 2))  # MA(1) model
summary(model_ma1)

#3.ii) Forecast the next 10 observations
forecast_ma1 <- forecast(model_ma1, h = 10)
forecast_ma1
# Plot the forecast
plot(forecast_ma1, main = "Forecast from MA(1) Model")

#1.c) Estimate the MA(1) parameter
theta_est <- coef(model_ma1)["ma1"]
cat("Estimated MA(1) parameter (θ):", theta_est, "\n")

