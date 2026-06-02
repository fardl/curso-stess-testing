#################################################
# CURSO:
# Stress Testing en Sistemas de Pensiones
# MACROPOL
#
# EJEMPLO:
# Stress Testing ARIMA
#################################################

#------------------------------------
# 1. Librerías
#------------------------------------

library(macropenstress)
library(forecast)
library(ggplot2)

#------------------------------------
# 2. Cargar datos
#------------------------------------

datos <- cargar_datos_pensiones()

macro <- datos$modelo

head(macro)

#------------------------------------
# 3. Construir serie temporal
#------------------------------------

y <- ts(
  macro$inflacion,
  frequency = 12
)

#------------------------------------
# 4. Estimar modelo ARIMA
#------------------------------------

modelo_arima <- auto.arima(y)

summary(modelo_arima)

#------------------------------------
# 5. Definir perfil del shock
#------------------------------------

k_h <- c(
  2.0,
  1.8,
  1.5,
  1.2,
  1.0,
  0.8,
  0.6,
  0.5,
  0.4,
  0.3,
  0.2,
  0.1
)

#------------------------------------
# 6. Construir escenario adverso
#------------------------------------

res_arima <- escenario_arima_shock(
  modelo_arima = modelo_arima,
  h = 12,
  k_h = k_h,
  nombre_variable = "Inflacion",
  n_hist = 24,
  fechas = macro$fecha
)

#------------------------------------
# 7. Resultados
#------------------------------------

head(res_arima$escenarios)

#------------------------------------
# 8. Gráfico
#------------------------------------

print(res_arima$grafico)

#------------------------------------
# 9. Exportar gráfico
#------------------------------------

ggsave(
  filename = "stress_test_arima.png",
  plot = res_arima$grafico,
  width = 10,
  height = 6,
  dpi = 300
)

#------------------------------------
# 10. Interpretación
#------------------------------------

cat("\n")
cat("=====================================\n")
cat(" INTERPRETACION ECONOMICA\n")
cat("=====================================\n")

cat("
El escenario baseline representa la
trayectoria esperada bajo condiciones
macroeconomicas normales.

El escenario adverso incorpora un
shock transitorio decreciente definido
por k_h.

La persistencia del shock depende de:
- estructura ARIMA
- dinamica autoregresiva
- perfil temporal del stress

Este enfoque permite construir
escenarios regulatorios para:
- inflacion
- tasas
- tipo de cambio
- crecimiento
- rentabilidad previsional
")
