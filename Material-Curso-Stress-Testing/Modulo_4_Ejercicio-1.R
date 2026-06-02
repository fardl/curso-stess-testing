# ==========================================
# BASE SINTÉTICA PARA STRESS TESTING AFP
# ==========================================

set.seed(123)
rm(list = ls())
# Número de observaciones
n <- 240   # 20 años mensuales

# Fechas
fechas <- seq(as.Date("2015-01-01"),
              by = "month",
              length.out = n)

# Factores macro-financieros
delta_tasa <- rnorm(n, mean = 0, sd = 0.25)

delta_tipo_cambio <- rnorm(n, mean = 0.3, sd = 2)

ret_equity_global <- rnorm(n, mean = 0.7, sd = 4)

delta_spread_soberano <- rnorm(n, mean = 0.05, sd = 0.40)

inflacion_mensual <- rnorm(n, mean = 4, sd = 0.2)

# ==========================================
# Generación de retorno del fondo previsional
# ==========================================

ret_fondo <- (
   -0.40 * delta_tasa +
    0.15 * delta_tipo_cambio +
    0.55 * ret_equity_global -
    0.35 * delta_spread_soberano -
    0.40 * inflacion_mensual +
    rnorm(n, 0, 0.05)
)

# ==========================================
# Base final
# ==========================================

datos <- data.frame(
  fecha = fechas,
  ret_port = ret_fondo,
  tasa = delta_tasa,
  fx = delta_tipo_cambio,
  equity = ret_equity_global,
  spread = delta_spread_soberano,
  inflacion = inflacion_mensual
)

# Ver primeras filas
head(datos)


# Modelo multifactorial 
mod_mf <- lm(ret_port ~ 0+tasa + equity + spread + inflacion,
             data = datos)

summary(mod_mf)


## Creacion de escenario 
# ==========================================
# ESCENARIOS MACRO-FINANCIEROS
# MÓDULO 3
# ==========================================
#Baseline: Normalizacion Economica + Vol. moderada+ Inflacion controlada
#Adverso: Shock Financiero+Depreciacion cambiaria + Caida accionaria + Incremento de Spreads
#Severo: Crisis sitemica + Stress monetario + Shock inflacionario + Colapso de mercados


escenarios_m3 <- data.frame(
  
  escenario = c(
    "baseline", 
    "baseline",
    "adverso",
    "adverso",
    "severo",
    "severo"
  ),
  
  fecha = as.Date(c(
    "2025-01-01",
    "2025-02-01",
    "2025-01-01",
    "2025-02-01",
    "2025-01-01",
    "2025-02-01"
  )),
  
  # Shock tasas (en %)
  tasa = c(
    0.10,
    0.15,
    1.50,
    2.00,
    3.00,
    4.00
  ),
  
  # Variación tipo de cambio (%)
  fx = c(
    0.5,
    0.8,
    8,
    12,
    20,
    25
  ),
  
  # Retorno equity (%)
  equity = c(
    1.5,
    2.0,
    -12,
    -18,
    -30,
    -40
  ),
  
  # Shock spread soberano (%)
  spread = c(
    0.10,
    0.15,
    1.20,
    1.80,
    3.00,
    4.50
  ),
  
  # Inflación mensual (%)
  inflacion = c(
    0.30,
    0.35,
    0.90,
    1.20,
    2.00,
    3.00
  )
  
)

escenarios_m3

escenarios_m3$rp_hat <- predict(
  mod_mf,
  newdata = escenarios_m3
)

aggregate(
  rp_hat ~ escenario,
  data = escenarios_m3,
  mean
)
