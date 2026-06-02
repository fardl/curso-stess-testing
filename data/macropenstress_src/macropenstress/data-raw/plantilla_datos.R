# Paquetes sugeridos
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(tsibble)
rm(list=ls())

setwd(r"(C:\Users\t490\Documents\MacroPol\stress-testing\data)")

datos <- read_excel("datos_macro_pensiones.xlsx") |>
  mutate(fecha = ymd(date)) |>
  arrange(fecha)


# Variables transformadas 
datos_modelo <- datos |>
  mutate(
    crecimiento = 100*(log(imae)-lag(log(imae),12)),
    inflacion = 100*(log(ipc)-lag(log(ipc),12)),
    deprec_fx = 100*(log(tipo_de_cambio) - lag(log(tipo_de_cambio),12)),
    ret_fondo = rentabilidad_afp,
    salario_real = 100*(log(salario_cotizable/ipc)-lag(log(salario_cotizable/ipc),12))
  ) |>
  tidyr::drop_na()

