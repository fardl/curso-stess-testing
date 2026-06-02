# ==========================================
# PAQUETES
# ==========================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ==========================================
# 1. BASE SINTÉTICA HISTÓRICA
# ==========================================

# ==========================================
# BASE SINTÉTICA CON VAR(1)
# ==========================================

set.seed(123)

n <- 120

fechas <- seq(as.Date("2015-01-01"),
              by = "month",
              length.out = n)

vars <- c("tasa", "fx", "equity", "spread", "inflacion")
K <- length(vars)

# Medias de largo plazo
mu <- c(
  tasa = 0.00,
  fx = 0.30,
  equity = 0.70,
  spread = 0.05,
  inflacion = 0.40
)

# Matriz de persistencia e interacciones
A <- matrix(c(
  0.85,  0.00,  0.00,  0.10,  0.10,
  0.05,  0.70, -0.05,  0.15,  0.10,
  -0.10, 0.00,  0.55, -0.20, -0.05,
  0.10,  0.05, -0.10,  0.85,  0.05,
  0.05,  0.10, -0.02,  0.05,  0.90
), nrow = K, byrow = TRUE)

rownames(A) <- vars
colnames(A) <- vars

# Matriz de covarianza de shocks
Sigma <- matrix(c(
  0.15^2,  0.01,   -0.02,   0.02,   0.005,
  0.01,    1.20^2, -0.60,   0.25,   0.10,
  -0.02,   -0.60,    2.50^2,-0.70,  -0.10,
  0.02,    0.25,   -0.70,   0.25^2, 0.02,
  0.005,   0.10,   -0.10,   0.02,   0.08^2
), nrow = K, byrow = TRUE)

rownames(Sigma) <- vars
colnames(Sigma) <- vars

# Para garantizar matriz positiva definida
Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat)

# Simulación VAR(1)
Y <- matrix(NA, nrow = n, ncol = K)
colnames(Y) <- vars

Y[1, ] <- mu

for (t in 2:n) {
  shock_t <- MASS::mvrnorm(1, mu = rep(0, K), Sigma = Sigma)
  
  Y[t, ] <- mu + A %*% (Y[t - 1, ] - mu) + shock_t
}

datos <- data.frame(
  fecha = fechas,
  Y
)

# Retorno del portafolio
datos$ret_port <- with(datos,
                       -0.40 * tasa +
                         0.15 * fx +
                         0.55 * equity -
                         0.35 * spread -
                         0.40 * inflacion +
                         rnorm(n, 0, 0.30)
)

head(datos)

# ==========================================
# 2. MODELO MULTIFACTORIAL
# ==========================================

mod_mf <- lm(ret_port ~ tasa + fx + equity + spread + inflacion,
             data = datos)

summary(mod_mf)

# ==========================================
# 3. ESCENARIOS A 24 MESES
# ==========================================
# ==========================================
# ESCENARIOS A 24 MESES RELATIVOS AL BASELINE
# ==========================================

h <- 24

fechas_proy <- seq(as.Date("2025-01-01"),
                   by = "month",
                   length.out = h)

baseline <- data.frame(
  fecha = fechas_proy,
  escenario = "baseline",
  t = 1:h,
  
  tasa = 0.10 + (0.25 - 0.10) * (1:h - 1) / (h - 1),
  fx = 0.50 + (0.30 - 0.50) * (1:h - 1) / (h - 1),
  equity = 4.50 + (1.00 - 1.50) * (1:h - 1) / (h - 1),
  spread = 0.10 + (0.15 - 0.10) * (1:h - 1) / (h - 1),
  inflacion = 0.30 + (0.35 - 0.30) * (1:h - 1) / (h - 1)
)

# Shocks adicionales sobre el baseline
shock_adverso <- data.frame(
  t = 1:h,
  tasa = seq(1.40, 0.50, length.out = h),
  fx = seq(10.0, 2.5, length.out = h),
  equity = seq(-20.0, -2.0, length.out = h),
  spread = seq(1.70, 0.45, length.out = h),
  inflacion = seq(0.90, 0.25, length.out = h)
)

shock_severo <- data.frame(
  t = 1:h,
  tasa = seq(3.90, 1.20, length.out = h),
  fx = seq(24.0, 5.0, length.out = h),
  equity = seq(-42.0, -4.0, length.out = h),
  spread = seq(4.40, 0.85, length.out = h),
  inflacion = seq(2.70, 0.55, length.out = h)
)

adverso <- baseline %>%
  left_join(shock_adverso, by = "t", suffix = c("_base", "_shock")) %>%
  transmute(
    fecha,
    escenario = "adverso",
    t,
    tasa = tasa_base + tasa_shock,
    fx = fx_base + fx_shock,
    equity = equity_base + equity_shock,
    spread = spread_base + spread_shock,
    inflacion = inflacion_base + inflacion_shock
  )

severo <- baseline %>%
  left_join(shock_severo, by = "t", suffix = c("_base", "_shock")) %>%
  transmute(
    fecha,
    escenario = "severo",
    t,
    tasa = tasa_base + tasa_shock,
    fx = fx_base + fx_shock,
    equity = equity_base + equity_shock,
    spread = spread_base + spread_shock,
    inflacion = inflacion_base + inflacion_shock
  )

escenarios_m3 <- bind_rows(
  baseline,
  adverso,
  severo
)

# Proyección del retorno del portafolio
escenarios_m3$rp_hat <- predict(
  mod_mf,
  newdata = escenarios_m3
)

escenarios_m3 <- escenarios_m3 %>%
  group_by(escenario) %>%
  arrange(fecha) %>%
  mutate(
    rp_acum = cumsum(rp_hat)
  ) %>%
  ungroup()


# ==========================================
# 6. RESUMEN POR ESCENARIO
# ==========================================

resumen_escenarios <- escenarios_m3 %>%
  group_by(escenario) %>%
  summarise(
    retorno_promedio = mean(rp_hat),
    retorno_minimo = min(rp_hat),
    retorno_acumulado_24m = sum(rp_hat),
    .groups = "drop"
  )

resumen_escenarios

# ==========================================
# 7. GRÁFICO: RETORNO MENSUAL PROYECTADO
# ==========================================

ggplot(escenarios_m3,
       aes(x = fecha, y = rp_hat, color = escenario)) +
  geom_line(linewidth = 1.1) +
  labs(
    title = "Retorno mensual proyectado del portafolio previsional",
    subtitle = "Modelo multifactorial aplicado a escenarios macro-financieros",
    x = NULL,
    y = "Retorno mensual estimado (%)",
    color = "Escenario"
  ) +
  theme_minimal()

# ==========================================
# 8. GRÁFICO: RETORNO ACUMULADO
# ==========================================

ggplot(escenarios_m3,
       aes(x = fecha, y = rp_acum, color = escenario)) +
  geom_line(linewidth = 1.1) +
  labs(
    title = "Retorno acumulado proyectado a 24 meses",
    subtitle = "Impacto acumulado por escenario de stress testing",
    x = NULL,
    y = "Retorno acumulado (%)",
    color = "Escenario"
  ) +
  theme_minimal()

# ==========================================
# 9. GRÁFICO DE FACTORES
# ==========================================

escenarios_largo <- escenarios_m3 %>%
  select(fecha, escenario, tasa, fx, equity, spread, inflacion) %>%
  pivot_longer(
    cols = c(tasa, fx, equity, spread, inflacion),
    names_to = "factor",
    values_to = "valor"
  )

ggplot(escenarios_largo,
       aes(x = fecha, y = valor, color = escenario)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ factor, scales = "free_y") +
  labs(
    title = "Trayectorias macro-financieras por escenario",
    subtitle = "Horizonte de proyección: 24 meses",
    x = NULL,
    y = "Valor del factor",
    color = "Escenario"
  ) +
  theme_minimal()



inflacion_ <- escenarios_m3 |>
  select(fecha, escenario, inflacion) |>
  pivot_wider(
    names_from = escenario,
    values_from = inflacion
  )



tasa_ <- escenarios_m3 |>
  select(fecha, escenario, tasa) |>
  pivot_wider(
    names_from = escenario,
    values_from = tasa
  )

spread_ <- escenarios_m3 |>
  select(fecha, escenario, spread) |>
  pivot_wider(
    names_from = escenario,
    values_from = spread
  )


retorno_ <- escenarios_m3 |>
  select(fecha, escenario, rp_hat) |>
  pivot_wider(
    names_from = escenario,
    values_from = rp_hat
  )

