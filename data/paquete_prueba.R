# Escenarios con modelos ARIMA y VAR

rm(list=ls())
library(macropenstress)
library(forecast)
library(lubridate)

datos <- cargar_datos_pensiones()
macro <- datos$datos_modelo


# Escenario ARIMA
inflacion <- ts(macro$inflacion, frequency = 12)
modelo_arima <- auto.arima(inflacion)

res_arima <- escenario_arima_shock(
  modelo_arima = modelo_arima,
  h = 12,
  k_h = c(2,1.8,1.5,1.2,1,0.8,0.6,0.5,0.4,0.3,0.2,0.1),
  nombre_variable = "Inflacion",
  n_hist = 24,
  fechas = macro$fecha
)

res_arima$escenarios
res_arima$grafico

# Arima tasa de interes nominal
interbank <- ts(macro$interbancaria, freq=12)
mod_interbank <- auto.arima(interbank)

res_arima_interbank <- escenario_arima_shock(
  modelo = mod_interbank,
  h =12, 
  k_h = c(rep(2,12)) ,
  nombre_variable = "Tasa Interbancaria",
  n_hist=24,
  fechas=macro$fecha
)
res_arima_interbank$escenarios
res_arima_interbank$grafico

# Tasa de interes real
tasa_real_baseline <- res_arima_interbank$escenarios['baseline'] - res_arima$escenarios['baseline']

tasa_real_adverso <- res_arima_interbank$escenarios['adverso'] - res_arima$escenarios['adverso']



# Escenario VAR con MonteCarlo/Percentiles/severidad
library(vars)
x <- macro[, c("inflacion", "interbancaria", "crecimiento")]
x <- na.omit(x)

modelo_var <- VAR(x, p=2, type="const")

res_var <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 5000,
  percentiles = c(0.05, 0.50, 0.95),
  enfoques = c("marginal")
)

res_var$resultados
res_var$graficos$panel

res_var <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 5000,
  enfoques = c("conjunta"),
  severity_by_path = mi_indice_severidad,
  severity_probs = c(0.95, 0.99)
)


## Trayectoria conjunta extrema

res_mc <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 10000,
  enfoques = c("marginal")
)

sim <- res_mc$simulaciones

inf_h  <- rowMeans(t(sim[, "inflacion", ]))
cre_h  <- rowMeans(t(sim[, "crecimiento", ]))
tas_h  <- rowMeans(t(sim[, "interbancaria", ]))

severity_index <-
  0.35 * as.numeric(scale(inf_h)) +
  0.35 * as.numeric(scale(tas_h)) -
  0.30 * as.numeric(scale(cre_h))
  
res_conjunta <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 10000,
  enfoques = c("conjunta"),
  severity_by_path = severity_index,
  severity_probs = c(0.99),
  conjunta_resumen = "mediana"
)

res_conjunta$graficos$panel


# VAR Condicional

# “La inflación permanece elevada durante 12 meses.”
condicion <- data.frame(
  inflacion = c(5.5, 5.8, 6.0, 6.2, 6.1, 6.0,
                5.9, 5.8, 5.7, 5.6, 5.5, 5.4)
)

res_condicional <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 5000,
  enfoques = c("condicional"),
  conditional_paths = list(inflacion_alta = condicion)
)

res_condicional$resultados
res_condicional$graficos$panel


# Concionar inflacion e interbancaria

condicion <- data.frame(
  inflacion = c(5.5, 5.8, 6.0, 6.2, 6.1, 6.0,
                5.9, 5.8, 5.7, 5.6, 5.5, 5.4),
  interbancaria = c(8.0, 8.5, 9.0, 9.5, 9.3, 9.0,
                    9.0, 9.0, 9.0,9.0, 9.0, 9.0)
)

res_condicional <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 5000,
  enfoques = c("condicional"),
  conditional_paths = list(inflacion_tasas_altas = condicion)
)

res_condicional$graficos$panel
