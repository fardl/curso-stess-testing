
library(macropenstress)
library(forecast)
library(vars)
library(dplyr)

base <- cargar_datos_pensiones()
datos_modelo <- base$datos_modelo

# ARIMA
infl_ts <- ts(datos_modelo$inflacion, start = c(2007, 10), frequency = 12)
fit_infl <- auto.arima(infl_ts, seasonal = FALSE)
k_h <- c(rep(1.5, 3), 1, 0.5, rep(0, 19))

esc_arima <- escenario_arima_shock(
  modelo_arima = fit_infl,
  k_h = k_h,
  nombre_variable = "Inflación",
  tasa_nominal = tail(datos_modelo$interbancaria, 1)
)
print(esc_arima$escenarios)
print(esc_arima$grafico)

# VAR
Y <- datos_modelo |>
  select(crecimiento, inflacion, interbancaria, deprec_fx, embi) |>
  as.matrix()

fit_var <- VAR(Y, p = 2, type = "const")

h <- 8
ultimo_embi <- tail(Y[, "embi"], 1)
sigma_fx <- sd(Y[, "deprec_fx"], na.rm = TRUE)

baseline_paths <- data.frame(
  embi = rep(ultimo_embi, h),
  deprec_fx = rep(2, h)
)

adverse_paths <- data.frame(
  embi = ultimo_embi + c(0.5, 1, 1.5, 2, 2.5, 2, 1.5, 1),
  deprec_fx = c(2, 2, 1.5, 1.5, 1, 1, 0.5, 0.5) * sigma_fx
)

severity_previsional <- function(path, sd_marg) {
  sum(
    -path[, "crecimiento"] / sd_marg["crecimiento"] +
      path[, "inflacion"] / sd_marg["inflacion"] +
      path[, "interbancaria"] / sd_marg["interbancaria"] +
      path[, "deprec_fx"] / sd_marg["deprec_fx"] +
      path[, "embi"] / sd_marg["embi"]
  )
}

esc_var <- escenario_var_stress(
  modelo_var = fit_var,
  h = h,
  M = 5000,
  enfoques = c("marginal", "conjunta", "condicional"),
  percentiles = c(0.50, 0.05, 0.01),
  severity_index = severity_previsional,
  severity_probs = c(0.50, 0.95, 0.99),
  conditional_paths = list(
    Baseline_condicionado = baseline_paths,
    Adverso_condicionado = adverse_paths
  )
)

print(esc_var$resultados)
print(esc_var$graficos$panel)
