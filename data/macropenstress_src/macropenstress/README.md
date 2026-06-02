
# macropenstress

Paquete didáctico para el curso **Stress Testing en Sistemas de Pensiones de Capitalización Individual** de MACROPOL.

## Contexto

Este paquete se ubica principalmente en el **Módulo 3: Construcción de Escenarios Macro-Financieros**, y conecta con el **Módulo 4: Stress Testing del Portafolio Previsional**. Permite diferenciar:

- **Baseline:** proyección central del modelo ARIMA o VAR.
- **Adverso:** desviación respecto al baseline mediante shocks calibrados, percentiles simulados, trayectoria conjunta extrema o condiciones impuestas.
- **Narrativa económica:** por ejemplo, depreciación cambiaria, aumento del EMBI, inflación persistente y deterioro del crecimiento.
- **Modelo cuantitativo:** ARIMA, VAR, Monte Carlo y VAR condicional.

## Instalación local

```r
install.packages("macropenstress_0.1.0.tar.gz", repos = NULL, type = "source")
```

O cargar desde la carpeta del paquete:

```r
devtools::load_all("macropenstress")
```

## Base de datos

```r
library(macropenstress)

base <- cargar_datos_pensiones()
datos_modelo <- base$datos_modelo
```

La base incluye las variables transformadas de `plantilla_datos.R`:

```r
crecimiento  = 100 * Δ12 log(imae)
inflacion    = 100 * Δ12 log(ipc)
deprec_fx    = 100 * Δ12 log(tipo_de_cambio)
ret_fondo    = rentabilidad_afp
salario_real = 100 * Δ12 log(salario_cotizable / ipc)
```

## Ejemplo ARIMA

```r
library(forecast)

infl_ts <- ts(datos_modelo$inflacion, start = c(2007, 10), frequency = 12)
fit_infl <- auto.arima(infl_ts, seasonal = FALSE)

k_h <- c(rep(1.5, 3), 1, 0.5, rep(0, 19))

esc_arima <- escenario_arima_shock(
  modelo_arima = fit_infl,
  k_h = k_h,
  nombre_variable = "Inflación",
  tasa_nominal = tail(datos_modelo$interbancaria, 1)
)

esc_arima$escenarios
esc_arima$grafico
```

## Ejemplo VAR con tres enfoques

```r
library(vars)

Y <- datos_modelo |>
  dplyr::select(crecimiento, inflacion, interbancaria, deprec_fx, embi) |>
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

custom_severity <- function(path, sd_marg) {
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
  severity_index = custom_severity,
  severity_probs = c(0.50, 0.95, 0.99),
  conditional_paths = list(Baseline_condicionado = baseline_paths,
                           Adverso_condicionado = adverse_paths)
)

esc_var$resultados
esc_var$graficos$panel
```

## Interpretación regulatoria

El paquete permite mostrar por qué un escenario adverso de pensiones no debe limitarse a percentiles marginales independientes. Para un supervisor previsional, la trayectoria conjunta extrema y el VAR condicional son más útiles para evaluar coherencia macrofinanciera: menor crecimiento, inflación alta, depreciación, aumento de tasas y EMBI deben ser compatibles dentro de una misma narrativa de transmisión hacia rentabilidad real, aportes, densidad de cotización y suficiencia de pensión.
