# ============================================================
# EJEMPLO SIMPLE: ARIMA + ESCENARIO ADVERSO DE INFLACIÓN
# ============================================================

library(forecast)
library(tidyverse)


# 1. Datos
source("plantilla_datos.R")

infl_ts <- ts(datos_modelo$inflacion,
              start = c(2007, 10),
              frequency = 12)

tasa_ts <- ts(datos_modelo$interbancaria,
              start = c(2007, 10),
              frequency = 12)

# 2. Modelo ARIMA y baseline
fit_infl <- auto.arima(infl_ts, seasonal = FALSE)

h <- 24
fc_infl <- forecast(fit_infl, h = h)

infl_baseline <- as.numeric(fc_infl$mean)

# 3. Shock adverso simple
sigma_infl <- sd(residuals(fit_infl), na.rm = TRUE)

shock <- c(
  rep(1.5, 3),  # shock fuerte
  1.0,          # empieza reversión
  0.5,          # se disipa
  rep(0, h - 5) # normalización
) * sigma_infl

infl_adversa <- infl_baseline + shock

# 4. Tasa real
# Supuesto simple: tasa nominal sigue su último valor observado
tasa_nominal <- rep(tail(tasa_ts, 1), h)

tasa_real_baseline <- tasa_nominal - infl_baseline
tasa_real_adversa  <- tasa_nominal - infl_adversa

# 5. Tabla final
escenario <- tibble(
  mes = 1:h,
  inflacion_baseline = infl_baseline,
  shock_inflacion = shock,
  inflacion_adversa = infl_adversa,
  tasa_nominal = tasa_nominal,
  tasa_real_baseline = tasa_real_baseline,
  tasa_real_adversa = tasa_real_adversa
)

print(escenario)

escenario_long <- escenario %>%
  select(mes, inflacion_baseline, inflacion_adversa) %>%
  pivot_longer(-mes,
               names_to = "escenario",
               values_to = "inflacion")

ggplot(escenario_long,
       aes(x = mes, y = inflacion, color = escenario)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Inflación: baseline vs escenario adverso",
    subtitle = "Adverso = baseline + shock de 1.5 desviaciones estándar",
    x = "Mes de proyección",
    y = "Inflación (%)"
  ) +
  theme_minimal()




# Gráfico tasa real: baseline vs adverso

escenario_real_long <- escenario %>%
  select(mes, tasa_real_baseline, tasa_real_adversa) %>%
  pivot_longer(
    cols = -mes,
    names_to = "escenario",
    values_to = "tasa_real"
  )

grafico <- ggplot(escenario_real_long,
       aes(x = mes, y = tasa_real, color = escenario)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Tasa real: baseline vs escenario adverso",
    subtitle = "Tasa real = tasa nominal - inflación",
    x = "Mes de proyección",
    y = "Tasa real aproximada (%)"
  ) +
  theme_minimal()

print(grafico)
