# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO MONTE CARLO VAR: TASA REAL NEGATIVA Y ESCENARIOS A/B/C
# ─────────────────────────────────────────────────────────────────────────────
rm(list=ls())
library(vars)
library(tidyverse)
library(MASS)
library(ggplot2)

source("plantilla_datos.R")

# ─────────────────────────────────────────────────────────────────────────────
# PASO 1: DATOS Y VAR
# ─────────────────────────────────────────────────────────────────────────────

Y <- datos_modelo |> 
  dplyr::select(inflacion, interbancaria) |> 
  as.matrix()

variables <- colnames(Y)

H <- 8
M <- 5000
p_var <- 2

set.seed(123)

fit_var <- VAR(Y, p = p_var, type = "const")

cat("\nRaíces del VAR:\n")
print(roots(fit_var))

fc_var <- predict(fit_var, n.ahead = H, ci = 0.95)
base_var <- sapply(fc_var$fcst, function(x) x[, "fcst"])

Sigma <- summary(fit_var)$covres
K <- ncol(Y)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 2: MONTE CARLO CON DINÁMICA VAR RECURSIVA
# ─────────────────────────────────────────────────────────────────────────────

B <- Bcoef(fit_var)

simulate_var_path <- function(fit_var, H, Sigma) {
  
  K <- fit_var$K
  p <- fit_var$p
  var_names <- colnames(fit_var$y)
  B <- Bcoef(fit_var)
  
  yhist <- tail(as.matrix(fit_var$y), p)
  colnames(yhist) <- var_names
  
  ysim <- rbind(
    yhist,
    matrix(NA, nrow = H, ncol = K)
  )
  
  colnames(ysim) <- var_names
  
  for (h in 1:H) {
    
    x <- rep(0, ncol(B))
    names(x) <- colnames(B)
    
    for (lag in 1:p) {
      for (v in var_names) {
        cname <- paste0(v, ".l", lag)
        if (cname %in% names(x)) {
          x[cname] <- ysim[p + h - lag, v]
        }
      }
    }
    
    if ("const" %in% names(x)) {
      x["const"] <- 1
    }
    
    shock <- MASS::mvrnorm(1, mu = rep(0, K), Sigma = Sigma)
    
    y_pred <- as.numeric(B %*% x)
    ysim[p + h, ] <- y_pred + shock
  }
  
  yout <- ysim[(p + 1):(p + H), ]
  colnames(yout) <- var_names
  
  return(yout)
}

sim_paths <- array(
  NA,
  dim = c(H, K, M),
  dimnames = list(
    horizonte = 1:H,
    variable = variables,
    simulacion = 1:M
  )
)

for (m in 1:M) {
  sim_paths[, , m] <- simulate_var_path(fit_var, H, Sigma)
}

# ─────────────────────────────────────────────────────────────────────────────
# PASO 3A: ENFOQUE A - PERCENTILES MARGINALES
# ─────────────────────────────────────────────────────────────────────────────
# Advertencia: útil para diagnóstico, no como escenario supervisor.

esc_A_p05 <- apply(sim_paths, c(1, 2), quantile, probs = 0.05)
esc_A_p50 <- apply(sim_paths, c(1, 2), quantile, probs = 0.50)
esc_A_p01 <- apply(sim_paths, c(1, 2), quantile, probs = 0.01)

colnames(esc_A_p05) <- variables
colnames(esc_A_p50) <- variables
colnames(esc_A_p01) <- variables

# ─────────────────────────────────────────────────────────────────────────────
# PASO 3B: ENFOQUE B - TRAYECTORIA CONJUNTA EXTREMA
# ─────────────────────────────────────────────────────────────────────────────
# Escenario adverso coherente: una trayectoria real simulada.

sd_marg <- apply(Y, 2, sd, na.rm = TRUE)

severity <- apply(sim_paths, 3, function(p) {
  sum(
      p[, "inflacion"]     / sd_marg["inflacion"] +
      p[, "interbancaria"] / sd_marg["interbancaria"] 
  )
})

m_p95 <- which.min(abs(severity - quantile(severity, 0.95)))
m_p99 <- which.min(abs(severity - quantile(severity, 0.99)))

esc_B_p95 <- sim_paths[, , m_p95]
esc_B_p99 <- sim_paths[, , m_p99]

# ─────────────────────────────────────────────────────────────────────────────
# PASO 3C: ENFOQUE C - SHOCKS CALIBRADOS A CRISIS
# ─────────────────────────────────────────────────────────────────────────────
# Narrativa: crisis de inflación, tasa interbancaria constante.

patron <- c(1.0, 1.0, 1.0, 1.0, 0.7, 0.5, 0.3, 0.2)

direccion <- c(
  inflacion     =  1,
  interbancaria =  0
)

sd_struct <- sqrt(diag(Sigma))
names(sd_struct) <- variables

# Crisis moderada: 3 sigma
shocks_crisis_3s <- t(sapply(seq_len(H), function(h) {
  3.0 * patron[h] * direccion * sd_struct
}))

# Crisis severa: 4.5 sigma
shocks_crisis_45s <- t(sapply(seq_len(H), function(h) {
  4.5 * patron[h] * direccion * sd_struct
}))

set.seed(456)

ruido_residual_3s <- MASS::mvrnorm(
  n = H,
  mu = rep(0, K),
  Sigma = 0.25 * Sigma
)

ruido_residual_45s <- MASS::mvrnorm(
  n = H,
  mu = rep(0, K),
  Sigma = 0.25 * Sigma
)

esc_C_3s <- base_var + shocks_crisis_3s + ruido_residual_3s
esc_C_45s <- base_var + shocks_crisis_45s + ruido_residual_45s

colnames(esc_C_3s) <- variables
colnames(esc_C_45s) <- variables

# ─────────────────────────────────────────────────────────────────────────────
# PASO 4: TASA REAL Y EVENTO SUPERVISOR
# ─────────────────────────────────────────────────────────────────────────────

calcular_indicadores <- function(escenario, nombre) {
  
  tasa_real <- escenario[, "interbancaria"] - escenario[, "inflacion"]
  
  evento_4t <- all(tasa_real[1:4] < 0)
  
  perdida_real_acum_4t <- sum(pmin(tasa_real[1:4], 0))
  
  tibble(
    escenario = nombre,
    prob_evento = as.numeric(evento_4t),
    tasa_real_min = min(tasa_real[1:4]),
    perdida_real_acum_4t = perdida_real_acum_4t
  )
}

# Probabilidad Monte Carlo del evento
tasa_real_mc <- sim_paths[, "interbancaria", ] - sim_paths[, "inflacion", ]

evento_mc_4t <- apply(tasa_real_mc[1:4, ], 2, function(x) all(x < 0))

prob_mc_4t <- mean(evento_mc_4t)

perdida_mc_4t <- apply(tasa_real_mc[1:4, ], 2, function(x) {
  sum(pmin(x, 0))
})

percentiles_perdida_mc <- quantile(
  perdida_mc_4t,
  probs = c(0.01, 0.05, 0.10, 0.50, 0.90, 0.95, 0.99)
)

resultados_escenarios <- bind_rows(
  calcular_indicadores(esc_A_p05,  "A - Percentil marginal 5%"),
  calcular_indicadores(esc_A_p01,  "A - Percentil marginal 1%"),
  calcular_indicadores(esc_B_p95,  "B - Trayectoria conjunta p95"),
  calcular_indicadores(esc_B_p99,  "B - Trayectoria conjunta p99"),
  calcular_indicadores(esc_C_3s,   "C - Crisis calibrada 3 sigma"),
  calcular_indicadores(esc_C_45s,  "C - Crisis calibrada 4.5 sigma")
)

cat("\nProbabilidad Monte Carlo de tasa real negativa por 4 trimestres:\n")
print(round(100 * prob_mc_4t, 2))

cat("\nPercentiles de pérdida real acumulada Monte Carlo:\n")
print(percentiles_perdida_mc)

cat("\nIndicadores por metodología:\n")
print(resultados_escenarios)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 5: ALERTA SUPERVISORA
# ─────────────────────────────────────────────────────────────────────────────

nivel_alerta <- case_when(
  prob_mc_4t >= 0.30 ~ "ALERTA ROJA",
  prob_mc_4t >= 0.15 ~ "ALERTA AMARILLA",
  TRUE               ~ "SIN ALERTA"
)

cat("\nNivel de alerta supervisora:\n")
print(nivel_alerta)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 6: BLOQUE DE GRÁFICOS CON HISTÓRICO CONTROLABLE
# ─────────────────────────────────────────────────────────────────────────────

n_hist_plot <- 24

T_total <- nrow(Y)

historico <- as.data.frame(Y) |>
  tail(n_hist_plot)

historico$periodo <- (T_total - n_hist_plot + 1):T_total
historico$escenario <- "Histórico"

historico_long <- historico |>
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "valor"
  )

make_long <- function(mat, nombre) {
  df <- as.data.frame(mat)
  colnames(df) <- variables
  df$periodo <- (T_total + 1):(T_total + H)
  df$escenario <- nombre
  
  df |>
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable",
      values_to = "valor"
    )
}

base_long <- make_long(base_var, "Baseline VAR")

A05_long <- make_long(esc_A_p05, "A - Marginal p5")
B99_long <- make_long(esc_B_p99, "B - Conjunta p99")
C3_long  <- make_long(esc_C_3s,  "C - Crisis 3 sigma")
C45_long <- make_long(esc_C_45s, "C - Crisis 4.5 sigma")

plot_data <- bind_rows(
  historico_long,
  base_long,
  A05_long,
  B99_long,
  C3_long,
  C45_long
)

grafico_escenarios <- ggplot(
  plot_data,
  aes(x = periodo, y = valor, color = escenario)
) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = T_total,
    linetype = "dashed"
  ) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Escenarios VAR: Percentiles marginales, trayectoria conjunta y crisis calibrada",
    subtitle = paste("Histórico mostrado:", n_hist_plot, "observaciones"),
    x = "Periodo",
    y = "Valor",
    color = ""
  )

print(grafico_escenarios)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 7: GRÁFICO DE TASA REAL POR ESCENARIO
# ─────────────────────────────────────────────────────────────────────────────

tasa_real_hist <- as.data.frame(Y) |>
  mutate(
    tasa_real = interbancaria - inflacion,
    periodo = seq_len(nrow(Y)),
    escenario = "Histórico"
  ) |>
  tail(n_hist_plot) |>
  select(periodo, tasa_real, escenario)

make_real <- function(mat, nombre) {
  tibble(
    periodo = (T_total + 1):(T_total + H),
    tasa_real = mat[, "interbancaria"] - mat[, "inflacion"],
    escenario = nombre
  )
}

tasa_real_plot <- bind_rows(
  tasa_real_hist,
  make_real(base_var, "Baseline VAR"),
  make_real(esc_A_p05, "A - Marginal p5"),
  make_real(esc_B_p99, "B - Conjunta p99"),
  make_real(esc_C_3s, "C - Crisis 3 sigma"),
  make_real(esc_C_45s, "C - Crisis 4.5 sigma")
)

grafico_tasa_real <- ggplot(
  tasa_real_plot,
  aes(x = periodo, y = tasa_real, color = escenario)
) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = T_total, linetype = "dashed") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Tasa real simulada por metodología",
    subtitle = "Tasa real aproximada = tasa interbancaria - inflación",
    x = "Periodo",
    y = "Tasa real",
    color = ""
  )

print(grafico_tasa_real)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 8: DISTRIBUCIÓN DE PÉRDIDA REAL ACUMULADA
# ─────────────────────────────────────────────────────────────────────────────

perdida_df <- tibble(
  perdida_real_acum_4t = perdida_mc_4t
)

grafico_perdida <- ggplot(
  perdida_df,
  aes(x = perdida_real_acum_4t)
) +
  geom_histogram(bins = 40) +
  geom_vline(
    xintercept = quantile(perdida_mc_4t, 0.05),
    linetype = "dashed"
  ) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribución Monte Carlo de pérdida real acumulada",
    subtitle = "Primeros cuatro trimestres",
    x = "Pérdida real acumulada",
    y = "Frecuencia"
  )

print(grafico_perdida)
