












































#2.1==== pratical Exponential and Moving Average Smoothing- Holts Winters Smoothing=====

##Q.1)
# Given time series data
data <- c(150.3, 150.9, 151.4, 151.9, 152.5, 152.9, 153.2, 153.7, 153.6, 153.5, 
          154.4, 154.9, 155.7, 156.3, 156.6, 156.7, 157, 157.3, 157.8, 158.3, 
          158.6, 158.6, 159.1, 159.3)


time_series_data <- ts(data, start = 1996, frequency = 1)

#a) Analyze given time series data acc to Simple Exponential Smoothing (SES) with α = 0.3
# Load necessary package
install.packages("forecast")
library(forecast)

ses_model <- ses(time_series_data, alpha = 0.3, h = 1)
summary(ses_model)

# Plot the SES result
plot(time_series_data, main = "Simple Exponential Smoothing (α = 0.3)", col = "blue", lwd = 2)
lines(fitted(ses_model), col = "red", lty = 2, lwd = 2)
legend("topleft", legend = c("Original Series", "SES Fitted"), col = c("blue", "red"), lty = 1:2)


#b)Holt's Exponential Smoothing with α = 0.3 and β = 0.2

# Holt's Exponential Smoothing with alpha = 0.3 and beta = 0.2
holt_model <- holt(time_series_data, alpha = 0.3, beta = 0.2, h = 1)

# Plot the Holt's result
plot(time_series_data, main = "Holt's Exponential Smoothing (α = 0.3, β = 0.2)", col = "blue", lwd = 2)
lines(fitted(holt_model), col = "green", lty = 2, lwd = 2)
legend("topleft", legend = c("Original Series", "Holt's Fitted"), col = c("blue", "green"), lty = 1:2)


# Plotting both models together for comparison
plot(time_series_data, main = "Comparison of SES and Holt's Methods", col = "blue", lwd = 2)
lines(fitted(ses_model), col = "red", lty = 2, lwd = 2)
lines(fitted(holt_model), col = "green", lty = 2, lwd = 2)
legend("topleft", legend = c("Original Series", "SES Fitted", "Holt's Fitted"), 
       col = c("blue", "red", "green"), lty = 1:2)


#Conclusion

#Trend Adaptation: Holt's model outperforms SES in tracking the overall trend of the 
#time series data. As the values are steadily increasing over the years, 
#Holt's model successfully captures this upward movement, while SES may lag behind due to 
#its constant smoothing approach.
#Flexibility: Holt's method is more flexible, making it suitable for data with trends. 
#SES, while simple and effective for stationary data, may not provide accurate forecasts 
#if the underlying data has a clear trend.




##Q.2)
#CO2 Moving Average
# Define the data
year <- 1991:2003
co2 <- c(355.62, 356.36, 357.1, 358.86, 360.9, 362.58, 363.84, 366.58, 368.3, 
         369.47, 371.03, 373.61, 357.61)
ts_co2 <- ts(co2, start = 1991, end = 2003)

# a) Time series plot
plot(ts_co2, type="o", col="blue", ylab="CO2 Concentration", main="CO2 Concentration from 1991-2003")

# b) 3-year moving average for forecasting
moving_avg <- filter(ts_co2, rep(1/3, 3), sides=2)
plot(moving_avg,,type="o",col="blue",)
forecast_2004 <- mean(tail(ts_co2, 3))  # Forecast for 2004 using the last 3 values
forecast_2004




##Q.3)===Apply holts-Winters method to AirPassenger data and forecast next 12 months dta
#Load the AirPassengers data
install.packages("forecast")
library(forecast)
data("AirPassengers")

plot(AirPassengers, main = "AirPassengers Data", ylab = "Number of Passengers", xlab = "Yea
r")
holt_winters_model <- HoltWinters(AirPassengers, gamma = TRUE, seasonal = "multiplicative")
forecasted_values <- forecast(holt_winters_model, h = 12)
forecasted_values
plot(forecasted_values, main = "Holt-Winters Forecast for AirPassengers", ylab = "Number of P
assengers", xlab = "Year")

