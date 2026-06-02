# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO MÓDULO 3: ARIMA INFLACIÓN + TASA NOMINAL
# Construcción didáctica de escenarios para stress testing previsional
# ─────────────────────────────────────────────────────────────────────────────

library(forecast)
library(ggplot2)
library(tidyverse)
rm(list=ls())
# ─────────────────────────────────────────────────────────────────────────────
# PARÁMETROS DIDÁCTICOS DEL EJERCICIO
# ─────────────────────────────────────────────────────────────────────────────

h <- 24                         # horizonte de pronóstico en meses
shock_max <- 1.5                # shock máximo: 1.5 desviaciones estándar
meses_shock_max <- 3            # meses con shock máximo
fecha_inicio_grafico <- 2023
fecha_fin_grafico <- 2027.5

cat("\n=== PARÁMETROS DEL EJERCICIO ===\n")
cat("Horizonte:", h, "meses\n")
cat("Shock máximo inflación:", shock_max, "desviaciones estándar\n")
cat("Duración shock máximo:", meses_shock_max, "meses\n")

# ─────────────────────────────────────────────────────────────────────────────
# PASO 1: CARGAR DATOS HISTÓRICOS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 1: DATOS HISTÓRICOS REALES ===\n")

source("plantilla_datos.R")

infl_ts <- ts(datos_modelo$inflacion,
              start = c(2007, 10), frequency = 12)

tasa_ts <- ts(datos_modelo$interbancaria,
              start = c(2007, 10), frequency = 12)

tasa_real_ts <- tasa_ts - infl_ts

cat("\nÚltimos 12 meses inflación:\n")
print(tail(infl_ts, 12))

cat("\nÚltimos 12 meses tasa nominal:\n")
print(tail(tasa_ts, 12))

cat("\nÚltimos 12 meses tasa real:\n")
print(tail(tasa_real_ts, 12))

# ─────────────────────────────────────────────────────────────────────────────
# PASO 2: ESTIMAR ARIMA PARA INFLACIÓN
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 2A: ESTIMACIÓN ARIMA - INFLACIÓN ===\n")

infl_ts_clean <- na.omit(infl_ts)

fit_arima_infl <- auto.arima(
  infl_ts_clean,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)

print(summary(fit_arima_infl))

sd_residuos_infl <- sd(residuals(fit_arima_infl), na.rm = TRUE)

cat("\nDesviación estándar de residuos inflación:",
    round(sd_residuos_infl, 3), "p.p.\n")

cat("\nDiagnóstico residuos inflación:\n")
checkresiduals(fit_arima_infl, plot = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 3: ESTIMAR ARIMA PARA TASA NOMINAL
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 2B: ESTIMACIÓN ARIMA - TASA NOMINAL ===\n")

tasa_ts_clean <- na.omit(tasa_ts)

fit_arima_tasa <- auto.arima(
  tasa_ts_clean,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)

print(summary(fit_arima_tasa))

sd_residuos_tasa <- sd(residuals(fit_arima_tasa), na.rm = TRUE)

cat("\nDesviación estándar de residuos tasa:",
    round(sd_residuos_tasa, 3), "p.p.\n")

cat("\nDiagnóstico residuos tasa:\n")
checkresiduals(fit_arima_tasa, plot = FALSE)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 4: GENERAR ESCENARIOS BASELINE
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 3: PRONÓSTICOS BASELINE ===\n")

fc_infl_baseline <- forecast(
  fit_arima_infl,
  h = h,
  level = c(50, 80, 95)
)

fc_tasa_baseline <- forecast(
  fit_arima_tasa,
  h = h,
  level = c(50, 80, 95)
)

infl_baseline_values <- as.numeric(fc_infl_baseline$mean)
tasa_baseline_values <- as.numeric(fc_tasa_baseline$mean)

tasa_real_baseline <- tasa_baseline_values - infl_baseline_values

baseline_table <- data.frame(
  mes = 1:h,
  horizonte = paste0("m+", 1:h),
  inflacion_baseline = round(infl_baseline_values, 3),
  tasa_baseline = round(tasa_baseline_values, 3),
  tasa_real_baseline = round(tasa_real_baseline, 3)
)

cat("\nBaseline primeros 12 meses:\n")
print(baseline_table[1:12, ])

# ─────────────────────────────────────────────────────────────────────────────
# PASO 5: CONSTRUIR ESCENARIO ADVERSO DE INFLACIÓN
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 4: CONSTRUCCIÓN DEL ESCENARIO ADVERSO ===\n")

# Multiplicadores de sigma:
# 1.5σ durante 3 meses, luego reversión gradual.
shock_infl_sigma <- c(
  rep(shock_max, meses_shock_max),
  1.0,
  0.8,
  0.5,
  rep(0, h - meses_shock_max - 3)
)

# Convertir shock de desviaciones estándar a puntos porcentuales.
shock_infl_pp <- shock_infl_sigma * sd_residuos_infl

# Escenario adverso de inflación.
adverso_infl <- infl_baseline_values + shock_infl_pp

# En este ejercicio la tasa nominal no reacciona.
# Esto permite aislar el efecto inflacionario sobre la tasa real.
adverso_tasa <- tasa_baseline_values

# Tasa real adversa.
tasa_real_adverso <- adverso_tasa - adverso_infl

cat("\nSupuesto central:\n")
cat("Inflación adversa = baseline + k * sigma_residuos_ARIMA\n")
cat("Shock máximo =", shock_max, "*", round(sd_residuos_infl, 3),
    "=", round(shock_max * sd_residuos_infl, 3), "p.p.\n")

# ─────────────────────────────────────────────────────────────────────────────
# PASO 5B: TABLA DIDÁCTICA DE SUPUESTOS VS RESULTADOS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 5B: SUPUESTOS VS RESULTADOS ===\n")

shock_table <- data.frame(
  mes = 1:h,
  horizonte = paste0("m+", 1:h),
  multiplicador_sigma = shock_infl_sigma,
  sigma_residuos = round(sd_residuos_infl, 3),
  shock_pp = round(shock_infl_pp, 3),
  inflacion_baseline = round(infl_baseline_values, 3),
  inflacion_adversa = round(adverso_infl, 3),
  tasa_nominal = round(adverso_tasa, 3),
  tasa_real_baseline = round(tasa_real_baseline, 3),
  tasa_real_adversa = round(tasa_real_adverso, 3)
)

cat("\nTabla didáctica primeros 12 meses:\n")
print(shock_table[1:12, ])

supuestos <- data.frame(
  supuesto = c(
    "Horizonte",
    "Variable estresada",
    "Shock máximo",
    "Duración shock máximo",
    "Reversión",
    "Tasa nominal adversa"
  ),
  valor = c(
    paste(h, "meses"),
    "Inflación",
    paste(shock_max, "desviaciones estándar"),
    paste(meses_shock_max, "meses"),
    "Gradual: 1.0σ, 0.8σ, 0.5σ, luego 0",
    "Igual al baseline"
  )
)

resultados <- data.frame(
  indicador = c(
    "Inflación máxima baseline",
    "Inflación máxima adversa",
    "Shock máximo en p.p.",
    "Tasa real mínima baseline",
    "Tasa real mínima adversa",
    "Deterioro máximo de tasa real"
  ),
  valor = round(c(
    max(infl_baseline_values),
    max(adverso_infl),
    max(shock_infl_pp),
    min(tasa_real_baseline),
    min(tasa_real_adverso),
    min(tasa_real_adverso) - min(tasa_real_baseline)
  ), 3)
)

cat("\nSupuestos:\n")
print(supuestos)

cat("\nResultados clave:\n")
print(resultados)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 6: ALERTA REGULATORIA AUTOMÁTICA
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 6: ALERTA REGULATORIA ===\n")

meses_tasa_real_negativa <- sum(tasa_real_adverso < 0)

if (meses_tasa_real_negativa > 0) {
  cat("ALERTA: La tasa real adversa cae por debajo de 0% en",
      meses_tasa_real_negativa, "mes(es).\n")
  cat("Implicación: presión sobre retorno real, salario real y suficiencia previsional.\n")
} else {
  cat("Sin alerta crítica: la tasa real adversa permanece positiva.\n")
}

# ─────────────────────────────────────────────────────────────────────────────
# PASO 7: VECTORES DE TIEMPO PARA GRÁFICOS
# ─────────────────────────────────────────────────────────────────────────────

tiempo_forecast <- seq(
  from = tail(time(infl_ts_clean), 1)[1] + 1 / 12,
  by = 1 / 12,
  length.out = h
)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 8: GRÁFICOS DIDÁCTICOS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 8: VISUALIZACIÓN ===\n")

# Gráfico 1: shock en puntos porcentuales
plot(
  1:h, shock_infl_pp,
  type = "h",
  lwd = 5,
  col = "red",
  main = "Shock adverso de inflación",
  xlab = "Mes de proyección",
  ylab = "Shock en puntos porcentuales"
)
points(1:h, shock_infl_pp, pch = 16, col = "red")
abline(h = 0, col = "black", lty = 3)
grid()

# Gráfico 2: inflación
plot(
  infl_ts_clean,
  main = "Inflación: observado, baseline y adverso",
  ylab = "Inflación (%)",
  xlab = "Tiempo",
  type = "l",
  lwd = 2,
  col = "darkblue",
  xlim = c(fecha_inicio_grafico, fecha_fin_grafico),
  ylim = range(c(infl_ts_clean, infl_baseline_values, adverso_infl), na.rm = TRUE)
)
grid()
lines(tiempo_forecast, infl_baseline_values, col = "darkgreen", lwd = 2.5)
lines(tiempo_forecast, adverso_infl, col = "red", lwd = 2.5)
points(time(tail(infl_ts_clean, 1)), tail(infl_ts_clean, 1),
       pch = 16, col = "darkblue")
legend(
  "topright",
  legend = c("Observado", "Baseline", "Adverso"),
  col = c("darkblue", "darkgreen", "red"),
  lwd = c(2, 2.5, 2.5),
  cex = 0.85
)

# Gráfico 3: tasa nominal
plot(
  tasa_ts_clean,
  main = "Tasa nominal: observado y baseline",
  ylab = "Tasa nominal (%)",
  xlab = "Tiempo",
  type = "l",
  lwd = 2,
  col = "darkgreen",
  xlim = c(fecha_inicio_grafico, fecha_fin_grafico),
  ylim = range(c(tasa_ts_clean, tasa_baseline_values), na.rm = TRUE)
)
grid()
lines(tiempo_forecast, tasa_baseline_values, col = "blue", lwd = 2.5)
legend(
  "bottomright",
  legend = c("Observado", "Baseline"),
  col = c("darkgreen", "blue"),
  lwd = c(2, 2.5),
  cex = 0.85
)

# Gráfico 4: tasa real
plot(
  tasa_real_ts,
  main = "Tasa real: baseline vs adverso",
  ylab = "Tasa real aproximada (%)",
  xlab = "Tiempo",
  type = "l",
  lwd = 2,
  col = "black",
  xlim = c(fecha_inicio_grafico, fecha_fin_grafico),
  ylim = range(c(tasa_real_ts, tasa_real_baseline, tasa_real_adverso), na.rm = TRUE)
)
grid()
abline(h = 0, col = "red", lty = 3, lwd = 1.5)
lines(tiempo_forecast, tasa_real_baseline, col = "blue", lwd = 2.5)
lines(tiempo_forecast, tasa_real_adverso, col = "red", lwd = 2.5)
legend(
  "topright",
  legend = c("Observado", "Baseline", "Adverso", "Umbral 0%"),
  col = c("black", "blue", "red", "red"),
  lwd = c(2, 2.5, 2.5, 1.5),
  lty = c(1, 1, 1, 3),
  cex = 0.85
)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 9: EXPORTAR RESULTADOS PARA USO EN MÓDULOS 4 Y 5
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 9: EXPORTAR RESULTADOS ===\n")

escenarios_final <- data.frame(
  fecha_indice = tiempo_forecast,
  mes = 1:h,
  horizonte = paste0("m+", 1:h),
  inflacion_baseline = infl_baseline_values,
  inflacion_adversa = adverso_infl,
  shock_inflacion_pp = shock_infl_pp,
  tasa_nominal_baseline = tasa_baseline_values,
  tasa_nominal_adversa = adverso_tasa,
  tasa_real_baseline = tasa_real_baseline,
  tasa_real_adversa = tasa_real_adverso
)

write.csv(
  escenarios_final,
  "escenarios_arima_inflacion_tasa_modulo3.csv",
  row.names = FALSE
)

cat("Archivo exportado: escenarios_arima_inflacion_tasa_modulo3.csv\n")

cat("\n✓ EJERCICIO COMPLETADO\n")
