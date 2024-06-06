#Ashishkumar Pemmaraju
#WIU ID: 923-19-4632
#Final Project R Code

#Importing Packages

install.packages("TSA")
install.packages("forecast")
library(TSA)
library(forecast)
library(tseries)
data(package = "TSA")
data(electricity)

##ARIMA MODELLING
#Allocating Data
elec <- electricity
elec
#Plotting time series of electricity production on monthly basis
plot(elec, main="Monthly Electricity Production in US")
#Autocorrelation Function Plot
plot(acf(elec, main = " ACF plot of Monthly Electricity Production"))
plot(pacf(elec, main = " PACF plot of Monthly Electricity Production"))
#ARIMA Modelling
elec_arima_fit<- auto.arima(elec, seasonal = FALSE,approximation = FALSE)
summary(elec_arima_fit)
fore_points <- 48
elec_forecast <- forecast(elec_arima_fit,h=fore_points)
elec_forecast
#Plotting Electricity Forecast
plot(elec_forecast, main="Electricity Production Forecast for 4 years using ARIMA Modelling")

##SARIMA MODELING
elec_sarima_fit<- auto.arima(elec, seasonal = TRUE,approximation = FALSE)
#Summary
summary(elec_sarima_fit)
#Forecasting
fore_points <- 48
elec_forecast_sarima <- forecast(elec_sarima_fit,h=fore_points)
elec_forecast_sarima
#Plotting Electricity Forecast
plot(elec_forecast_sarima, main="Electricity Production Forecast for 4 years using SARIMA Modelling")

##ARCH and GARCH MODELLING
#Importing Packages
install.packages("astsa")
install.packages("forecast")
install.packages("rugarch")
data(package = "astsa")
library(forecast)
library(astsa)
library(rugarch)
library(tseries)
data(nyse)
nyse <- nyse
nyse
#ARCH and GARCH Model Fitting
acf(nyse)
nyse_arch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 4)))
nyse_arch_fit <- ugarchfit(nyse_arch,nyse)
nyse_arch_fit
#Summary
nyse_val <- nyse_arch_fit@fit
nyse_val
#Auto-Correlation Function
acf(na.omit(residuals(nyse_arch_fit)), main = "ACF plot")
#Forecasting for 4 years
forecast_points <- 48  
forecast_arch <- ugarchforecast(nyse_arch_fit, n.ahead = forecast_points)
#Plot of forecast
plot(forecast_arch,which=1, main = "Forecast of Returns on New York Stock Exchange")
#Summary of forecast values
print(forecast_arch)


##STATESPACE MODEL
platelet <- PLT
platelet
#State space Modelling
platelet_ss <- StructTS(platelet)
platelet_ss
residuals_platelet <- residuals(platelet_ss)
residuals_platelet
#Auto-Correlation Function
acf(residuals_platelet, main = "ACF plot of residuals from State Space Model")
#Forecast for 4 years that is 48 months
forecast_points <- 48
forecast_statespace_plt <- forecast(platelet_ss, h = forecast_points)
#Forecast plot
plot(forecast_statespace_plt, main = "Forecast of U.S unemployment using State Space Model")
#Summary of forecast values
print(forecast_statespace)



