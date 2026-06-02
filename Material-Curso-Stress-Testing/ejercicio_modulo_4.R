# ============================================================
# EJERCICIO INICIAL MÓDULO 4
# Modelo de factores usando solo la base macro
# ============================================================

rm(list=ls())

library(tidyverse)
library(lubridate)
library(rugarch)
library(broom)
library(purrr)

set.seed(1234)

setwd(r'(C:\Users\t490\Documents\MacroPol\stress-testing\data)')

# ============================================================
# 1. Cargar datos
# ============================================================
macro <- read.csv("datos_macro.csv")
escenarios <- read.csv("escenarios.csv")

# ============================================================
# 2. Preparar base histórica
# ============================================================
macro <- macro %>%
  mutate(
    date = mdy(fecha)
  ) %>%
  arrange(date) %>%
  mutate(
    inflacion =  INFLACION,
    deprec_fx =  DTCN,
    crecimiento =  DIMAE,
    embi = EMBI,
    interbancaria = TASA_PASIVA ,
    retorno_afp = RETORNOS,
  ) %>%
  drop_na(retorno_afp, inflacion, deprec_fx, crecimiento,
          embi, interbancaria)


# ============================================================
# 3. Estimar modelo de factores
# ============================================================
modelo_factores <- lm(
  retorno_afp ~ crecimiento +
    inflacion +
    #deprec_fx +
    embi +
    interbancaria,
  data = macro
)

summary(modelo_factores)
# ============================================================
# 4. Interpretar betas
# ============================================================
betas <- broom::tidy(modelo_factores)

betas
# ============================================================
# 5.  Predecir retorno AFP bajo cada escenario
# ============================================================
escenarios_modelo <- escenarios %>%
  filter(escenario != "histórico")%>%
  mutate(
    retorno_estimado = predict(
      modelo_factores,
      newdata = .
    )
  )
# ============================================================
# 7. Resumen por escenario
# ============================================================
resumen_escenarios <- escenarios_modelo %>%
  dplyr::select(c(periodo, escenario, retorno_estimado))%>%
  pivot_wider(names_from = escenario,
              values_from = retorno_estimado)
  
resumen_escenarios

# ============================================================
# 9. Gráfico de retornos estimados por escenario
# ============================================================
ggplot(escenarios_modelo,
       aes(x = periodo,
           y = retorno_estimado,
           color = escenario,
           group = escenario)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Retorno AFP estimado bajo escenarios macro-financieros",
    x = "Período",
    y = "Retorno estimado"
  )


# ============================================================
# 10. GARCH SOBRE LOS RESIDUOS DEL MODELO MULTIFACTORIAL
# ============================================================

macro$residuo_factores <- residuals(modelo_factores)

# Evaluacion de los residuos
acf(macro$residuo_factores)
pacf(macro$residuo_factores)

Box.test(
  macro$residuo_factores,
  lag = 12,
  type = "Ljung"
)

library(FinTS)
FinTS::ArchTest(
  macro$residuo_factores,
  lags = 12
)


spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
  mean.model = list(armaOrder = c(0, 0),include.mean = FALSE),
  distribution.model="std"
)

fit_garch <- ugarchfit(
  spec = spec_garch,
  data = macro$residuo_factores
)

show(fit_garch)

# Volatilidad condicional histórica
macro$vol_condicional <- as.numeric(sigma(fit_garch))

ggplot(macro, aes(x = date, y = vol_condicional)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Volatilidad condicional de los residuos del modelo multifactorial",
    x = "Fecha",
    y = "Volatilidad condicional"
  )


# ============================================================
# 11. SIMULACIÓN MONTE CARLO CON GARCH
# ============================================================

n_sim <- 10000
h_mc <- nrow(escenarios_modelo)

# Última volatilidad condicional estimada
sigma_t <- tail(sigma(fit_garch), 1)

# Parámetros GARCH estimados
coef_garch <- coef(fit_garch)

omega <- coef_garch["omega"]
alpha <- coef_garch["alpha1"]
beta  <- 0
shape <- NULL


# Función para simular residuos GARCH
# Función para simular residuos GARCH(1,1)
simular_residuos_garch <- function(h, omega, alpha, beta, sigma0,
                                   shape = NULL,
                                   shock_scale = 1) {
  
  eps <- numeric(h)
  sig2 <- numeric(h)
  
  sig2[1] <- (as.numeric(sigma0) * shock_scale)^2
  
  if (is.null(shape)) {
    z <- rnorm(h)
  } else {
    z <- rt(h, df = shape)
    z <- z / sqrt(shape / (shape - 2))
  }
  
  for (t in seq_len(h)) {
    
    if (t > 1) {
      sig2[t] <- omega + alpha * eps[t - 1]^2 + beta * sig2[t - 1]
    }
    
    eps[t] <- sqrt(sig2[t]) * z[t]
  }
  
  eps
}



# Simulación por escenario
sigma0 <- tail(as.numeric(sigma_t), 1)

sim_mc <- escenarios_modelo %>%
  dplyr::select(periodo, escenario, retorno_estimado) %>%
  dplyr::group_by(escenario) %>%
  dplyr::group_modify(~ {
    
    escenario_i <- .y$escenario
    
    escala_vol <- dplyr::case_when(
      escenario_i == "Baseline VAR" ~ 2,
      escenario_i == "adverso" ~ 4,
      escenario_i == "muy_adverso" ~ 6,
      TRUE ~ 1
    )
    
    retorno_base <- .x$retorno_estimado
    h <- length(retorno_base)
    
    matriz_sim <- replicate(
      n_sim,
      retorno_base +
        simular_residuos_garch(
          h = h,
          omega = omega,
          alpha = alpha,
          beta = beta,
          sigma0 = sigma0,
          shape = shape,
          shock_scale = escala_vol
        )
    )
    
    tibble::tibble(
      periodo = rep(.x$periodo, times = n_sim),
      simulacion = rep(seq_len(n_sim), each = h),
      retorno_simulado = as.vector(matriz_sim)
    )
  }) %>%
  dplyr::ungroup()

data<- sim_mc %>%
  filter(escenario=="muy_adverso")
hist(data$retorno_simulado, breaks = 200)




# ============================================================
# 12. CÁLCULO DE PÉRDIDAS, VaR Y EXPECTED SHORTFALL
# ============================================================

metricas_riesgo <- sim_mc %>%
  group_by(escenario, periodo) %>%
  summarise(
    retorno_promedio = mean(retorno_simulado),
    
    VaR_95 = quantile(retorno_simulado, 0.05),
    ES_95 = mean(
      retorno_simulado[
        retorno_simulado <= quantile(retorno_simulado, 0.05)
      ]
    ),
    
    VaR_99 = quantile(retorno_simulado, 0.01),
    ES_99 = mean(
      retorno_simulado[
        retorno_simulado <= quantile(retorno_simulado, 0.01)
      ]
    ),
    
    min_retorno = min(retorno_simulado),
    .groups = "drop"
  )
metricas_riesgo


# ============================================================
# 13. GRÁFICOS DE DISTRIBUCIÓN DE PÉRDIDAS
# ============================================================

ggplot(sim_mc, aes(x = retorno_simulado, fill = escenario)) +
  geom_density(alpha = 0.35) +
  theme_minimal() +
  labs(
    title = "Distribución simulada de pérdidas por escenario",
    x = "Pérdida simulada",
    y = "Densidad"
  )

ggplot(metricas_riesgo,
       aes(x = periodo, y = VaR_99,
           color = escenario,
           group = escenario)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "VaR 99% por escenario",
    x = "Período",
    y = "VaR 99%"
  )

ggplot(metricas_riesgo,
       aes(x = periodo, y = ES_99,
           color = escenario,
           group = escenario)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Expected Shortfall 99% por escenario",
    x = "Período",
    y = "Expected Shortfall 99%"
  )


# ============================================================
# 14. EXPORTAR RESULTADOS
# ============================================================

write.csv(
  metricas_riesgo,
  "metricas_riesgo_var_es_garch_montecarlo.csv",
  row.names = FALSE
)

write.csv(
  sim_mc,
  "simulaciones_montecarlo_garch.csv",
  row.names = FALSE
)

