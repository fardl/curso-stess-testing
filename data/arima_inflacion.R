#Implementacion: ARIMA para inflacion
rm(list=ls())
library(forecast)
library(ggplot2)
library(ggfortify)

setwd("~/MacroPol/stress-testing/data")

source("plantilla_datos.R")

# Serie mensual de inflacion
infl_ts <- ts(datos_modelo$inflacion,  start = c(2007,10), frequency=12)

plot(infl_ts, main="Inflacion Interanual")

# Seleccion automatica, pero revisada por el analista
fit_arima <- auto.arima(infl_ts, seasonal=FALSE)


summary(fit_arima)
checkresiduals(fit_arima)

# Proyeccion baseline
fc_base <- forecast(fit_arima, h = 24, level = c(50, 80,95))

plot(fc_base, showgap = FALSE, include=100)

