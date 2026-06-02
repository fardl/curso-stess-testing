# ============================================================
# TALLER: ESCENARIOS MACRO-FINANCIEROS
# Stress Testing en Pensiones
# Horizonte: 24 meses
# ============================================================

rm(list = ls())

# ------------------------------------------------------------
# 00_paquetes.R
# ------------------------------------------------------------

paquetes <- c(
  "readr", "dplyr", "tidyr", "lubridate",
  "forecast", "vars", "zoo", "ggplot2", "MASS"
)

instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(instalar) > 0) install.packages(instalar)

lapply(paquetes, library, character.only = TRUE)

set.seed(12345)

# ------------------------------------------------------------
# 01_datos.R
# ------------------------------------------------------------

datos_raw <- readr::read_csv("datos_macro_pensiones.csv")

datos <- datos_raw %>%
  dplyr::mutate(
    fecha = lubridate::mdy(date)
  ) %>%
  dplyr::arrange(fecha)

datos_modelo <- datos %>%
  dplyr::mutate(
    crecimiento = 100 * (log(imae) - dplyr::lag(log(imae), 12)),
    inflacion = 100 * (log(ipc) - dplyr::lag(log(ipc), 12)),
    deprec_fx = 100 * (log(tipo_de_cambio) - dplyr::lag(log(tipo_de_cambio), 12)),
    interbancaria = interbancaria,
    embi = embi
  ) %>%
  dplyr::select(
    fecha,
    crecimiento,
    inflacion,
    interbancaria,
    deprec_fx,
    embi
  ) %>%
  tidyr::drop_na()

variables_var <- c(
  "crecimiento",
  "inflacion",
  "interbancaria",
  "deprec_fx",
  "embi"
)

h <- 24

fechas_escenario <- seq(
  from = max(datos_modelo$fecha) %m+% months(1),
  by = "month",
  length.out = h
)

# ------------------------------------------------------------
# 02_arima.R
# Baseline univariado
# ------------------------------------------------------------

base_arima <- data.frame(fecha = fechas_escenario)

for (v in variables_var) {
  serie <- ts(datos_modelo[[v]], frequency = 12)
  modelo_arima <- forecast::auto.arima(serie)
  pred_arima <- forecast::forecast(modelo_arima, h = h)
  
  base_arima[[v]] <- as.numeric(pred_arima$mean)
}

base_arima <- base_arima %>%
  dplyr::mutate(
    escenario = "baseline_arima",
    fuente = "ARIMA univariado"
  )

# ------------------------------------------------------------
# 03_var.R
# Baseline multivariado
# ------------------------------------------------------------

datos_var <- datos_modelo %>%
  dplyr::select(dplyr::all_of(variables_var))

lag_optimo <- vars::VARselect(
  datos_var,
  lag.max = 6,
  type = "const"
)$selection["AIC(n)"]

lag_optimo <- as.integer(lag_optimo)

modelo_var <- vars::VAR(
  datos_var,
  p = lag_optimo,
  type = "const"
)

pred_var <- predict(
  modelo_var,
  n.ahead = h
)

base_var <- data.frame(fecha = fechas_escenario)

for (v in variables_var) {
  base_var[[v]] <- pred_var$fcst[[v]][, "fcst"]
}

base_var <- base_var %>%
  dplyr::mutate(
    escenario = "baseline_var",
    fuente = "VAR multivariado"
  )

# ------------------------------------------------------------
# Función auxiliar: simulación dinámica desde VAR
# ------------------------------------------------------------
simular_var <- function(modelo_var, datos_var, h = 24, n_sim = 5000) {
  
  variables <- colnames(datos_var)
  K <- length(variables)
  p <- modelo_var$p
  
  coefs <- vars::Bcoef(modelo_var)
  Sigma <- summary(modelo_var)$covres
  
  datos_var <- as.data.frame(datos_var)
  y_inicial <- as.matrix(tail(datos_var[, variables], p))
  
  sims <- array(
    NA_real_,
    dim = c(h, K, n_sim),
    dimnames = list(NULL, variables, NULL)
  )
  
  for (s in 1:n_sim) {
    
    y_hist <- y_inicial
    
    for (tt in 1:h) {
      
      x <- numeric(0)
      
      for (lag in 1:p) {
        x <- c(x, as.numeric(y_hist[nrow(y_hist) - lag + 1, variables]))
      }
      
      x <- c(x, 1)
      
      y_det <- as.numeric(coefs %*% x)
      
      shock <- MASS::mvrnorm(
        n = 1,
        mu = rep(0, K),
        Sigma = Sigma
      )
      
      y_next <- y_det + shock
      
      sims[tt, , s] <- y_next
      y_hist <- rbind(y_hist, y_next)
    }
  }
  
  return(sims)
}
# ------------------------------------------------------------
# 04_montecarlo.R
# Monte Carlo basado en VAR
# ------------------------------------------------------------

n_sim <- 5000

sims_var <- simular_var(
  modelo_var = modelo_var,
  datos_var = datos_var,
  h = h,
  n_sim = n_sim
)

mc_p05 <- data.frame(fecha = fechas_escenario)
mc_p50 <- data.frame(fecha = fechas_escenario)
mc_p95 <- data.frame(fecha = fechas_escenario)

for (v in variables_var) {
  
  mc_p05[[v]] <- apply(
    sims_var[, v, ],
    1,
    quantile,
    probs = 0.05,
    na.rm = TRUE
  )
  
  mc_p50[[v]] <- apply(
    sims_var[, v, ],
    1,
    quantile,
    probs = 0.50,
    na.rm = TRUE
  )
  
  mc_p95[[v]] <- apply(
    sims_var[, v, ],
    1,
    quantile,
    probs = 0.95,
    na.rm = TRUE
  )
}

mc_p05 <- mc_p05 %>%
  dplyr::mutate(
    escenario = "montecarlo_p05",
    fuente = "VAR + Monte Carlo"
  )

mc_p50 <- mc_p50 %>%
  dplyr::mutate(
    escenario = "montecarlo_p50",
    fuente = "VAR + Monte Carlo"
  )

mc_p95 <- mc_p95 %>%
  dplyr::mutate(
    escenario = "montecarlo_p95",
    fuente = "VAR + Monte Carlo"
  )

# ------------------------------------------------------------
# 05_choques.R
# Escenario histórico basado en variables VAR
# ------------------------------------------------------------

datos_stress <- datos_modelo %>%
  dplyr::mutate(
    stress_score =
      0.25 * inflacion +
      0.20 * interbancaria +
      0.20 * deprec_fx +
      0.20 * embi -
      0.15 * crecimiento
  )

datos_stress <- datos_stress %>%
  dplyr::mutate(
    stress_24m = zoo::rollapply(
      stress_score,
      width = h,
      FUN = mean,
      align = "left",
      fill = NA,
      na.rm = TRUE
    )
  )

inicio_peor <- which.max(datos_stress$stress_24m)

adv_hist <- datos_stress %>%
  dplyr::slice(inicio_peor:(inicio_peor + h - 1)) %>%
  dplyr::select(
    fecha,
    dplyr::all_of(variables_var)
  ) %>%
  dplyr::mutate(
    fecha = fechas_escenario,
    escenario = "adverso_historico",
    fuente = "peor ventana histórica 24m con índice VAR"
  )

# ------------------------------------------------------------
# Escenario hipotético: baseline VAR + shocks narrativos
# ------------------------------------------------------------

adv_hip <- base_var %>%
  dplyr::select(
    fecha,
    dplyr::all_of(variables_var)
  ) %>%
  dplyr::mutate(
    crecimiento = crecimiento + c(
      rep(-3.0, 6),
      rep(-4.0, 6),
      rep(-2.0, 6),
      rep(-1.0, 6)
    ),
    
    inflacion = inflacion + c(
      rep(2.5, 6),
      rep(4.0, 6),
      rep(3.0, 6),
      rep(1.5, 6)
    ),
    
    interbancaria = interbancaria + c(
      rep(2.5, 6),
      rep(4.0, 6),
      rep(3.0, 6),
      rep(1.5, 6)
    ),
    
    deprec_fx = deprec_fx + c(
      rep(10.0, 6),
      rep(8.0, 6),
      rep(5.0, 6),
      rep(3.0, 6)
    ),
    
    embi = embi + c(
      rep(2, 6),
      rep(3.50, 6),
      rep(2.50, 6),
      rep(1.50, 6)
    ),
    
    escenario = "adverso_hipotetico",
    fuente = "VAR baseline + shock hipotético"
  )

# ------------------------------------------------------------
# 06_exportar.R
# Matriz final en formato largo
# ------------------------------------------------------------

escenarios <- dplyr::bind_rows(
  base_arima,
  base_var,
  mc_p05,
  mc_p50,
  mc_p95,
  adv_hist,
  adv_hip
) %>%
  tidyr::pivot_longer(
    cols = dplyr::all_of(variables_var),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  dplyr::mutate(
    modulo_origen = "M3",
    fecha_generacion = Sys.Date(),
    unidad = dplyr::case_when(
      variable %in% c(
        "crecimiento",
        "inflacion",
        "interbancaria",
        "deprec_fx"
      ) ~ "porcentaje",
      variable == "embi" ~ "puntos_base",
      TRUE ~ "sin_unidad"
    )
  ) %>%
  dplyr::select(
    fecha,
    escenario,
    variable,
    valor,
    unidad,
    fuente,
    modulo_origen,
    fecha_generacion
  )

dir.create("outputs", showWarnings = FALSE)

readr::write_csv(
  escenarios,
  "outputs/escenarios_macrofinancieros.csv"
)

print(head(escenarios, 30))

# ------------------------------------------------------------
# Validación visual
# ------------------------------------------------------------

ggplot(
  escenarios,
  aes(
    x = fecha,
    y = valor,
    linetype = escenario
  )
) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Matriz de escenarios macro-financieros",
    subtitle = "ARIMA, VAR, Monte Carlo VAR y escenarios adversos",
    x = "Fecha",
    y = "Valor"
  )
