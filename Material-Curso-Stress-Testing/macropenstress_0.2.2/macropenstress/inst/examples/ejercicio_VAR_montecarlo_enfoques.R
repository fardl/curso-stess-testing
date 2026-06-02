# ─────────────────────────────────────────────────────────────────────────────
# MACROPOL — Stress Testing en Sistemas de Capitalización Individual
# Módulo 3 — Tres enfoques para construir escenarios sobre el mismo VAR
#
# Objetivo pedagógico:
#   (A) Percentiles marginales       -> incoherente económicamente (slide 30)
#   (B) Trayectoria conjunta extrema -> coherente, misma distribución
#   (C) Shocks calibrados a crisis   -> coherente, distribución DISTINTA
#
# Variables: crecimiento, inflacion, interbancaria, deprec_fx, embi
# ─────────────────────────────────────────────────────────────────────────────
rm(list=ls())
library(vars)
library(ggplot2)
library(tidyverse)
library(MASS)

source("plantilla_datos.R")

set.seed(20260501)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 1: ESTIMAR VAR (idéntico a tu script original)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 1: ESTIMACIÓN DEL VAR ===\n")

Y <- datos_modelo |>
  dplyr::select(crecimiento, inflacion, interbancaria, deprec_fx, embi) |>
  as.matrix()

print(VARselect(Y, lag.max = 6, type = "const"))

fit_var <- VAR(Y, p = 2, type = "const")
cat("\nEstabilidad del VAR (raíces deben ser < 1):\n")
print(roots(fit_var))

# Pronóstico baseline multivariado
fc_var   <- predict(fit_var, n.ahead = 8, ci = 0.95)
base_var <- sapply(fc_var$fcst, function(x) x[, "fcst"])

# ─────────────────────────────────────────────────────────────────────────────
# PASO 2: SIMULACIÓN MONTE CARLO (5,000 trayectorias)
# ─────────────────────────────────────────────────────────────────────────────

B     <- Bcoef(fit_var)
Sigma <- summary(fit_var)$covres
K     <- ncol(Y)
H     <- 8
M     <- 5000

variables <- colnames(Y)

sim_paths <- array(NA, dim = c(H, K, M),
                   dimnames = list(NULL, variables, NULL))

for (m in 1:M) {
  for (h in 1:H) {
    shock <- mvrnorm(1, mu = rep(0, K), Sigma = Sigma)
    sim_paths[h, , m] <- base_var[h, ] + shock
  }
}

# =============================================================================
# ENFOQUE A: PERCENTILES MARGINALES (lo del slide 30 — incoherente)
# =============================================================================

A_p50 <- apply(sim_paths, c(1, 2), quantile, probs = 0.50)
A_p05 <- apply(sim_paths, c(1, 2), quantile, probs = 0.05)
A_p01 <- apply(sim_paths, c(1, 2), quantile, probs = 0.01)
colnames(A_p50) <- colnames(A_p05) <- colnames(A_p01) <- variables

# =============================================================================
# ENFOQUE B: TRAYECTORIA CONJUNTA EXTREMA (coherente, misma distribución)
# =============================================================================
# Definimos una función de pérdida relevante para un fondo de pensiones.
# Aquí: índice de adversidad macro = caída del crecimiento + alza de inflación
#       + alza de tasa interbancaria + depreciación + alza de EMBI.
# Cada componente se estandariza por su sigma marginal de la simulación
# para que ninguna variable domine por escala.

sd_marg <- apply(sim_paths, 2, sd)

severity_by_path <- apply(sim_paths, 3, function(p) {
  # Mayor valor => escenario más adverso
  sum(
    -p[, "crecimiento"]   / sd_marg["crecimiento"]   +
      p[, "inflacion"]     / sd_marg["inflacion"]     +
      p[, "interbancaria"] / sd_marg["interbancaria"] +
      p[, "deprec_fx"]     / sd_marg["deprec_fx"]     +
      p[, "embi"]          / sd_marg["embi"]
  )
})

pick_path <- function(scores, prob) {
  target <- quantile(scores, prob)
  which.min(abs(scores - target))
}

m_B_p50 <- pick_path(severity_by_path, 0.50)
m_B_p95 <- pick_path(severity_by_path, 0.95)  # 95% de severidad = p5 adverso
m_B_p99 <- pick_path(severity_by_path, 0.99)  # 99% de severidad = p1 adverso

B_p50 <- sim_paths[, , m_B_p50]
B_p05 <- sim_paths[, , m_B_p95]
B_p01 <- sim_paths[, , m_B_p99]

# =============================================================================
# ENFOQUE C: SHOCKS CALIBRADOS A CRISIS (coherente, distribución DISTINTA)
# =============================================================================
# Calibración inspirada en la crisis bancaria dominicana 2003-2004:
#   - Caída persistente del crecimiento
#   - Repunte fuerte de inflación (pass-through cambiario)
#   - Endurecimiento monetario (alza de interbancaria)
#   - Depreciación abrupta del peso
#   - Salto del EMBI (riesgo soberano)
#
# Magnitudes: -3 sigma sostenido los primeros 4 trimestres, atenuación después

sd_struct <- sqrt(diag(Sigma))   # desvíos estándar de los shocks estructurales

# Patrón temporal del shock (factor multiplicativo por horizonte)
patron <- c(1.0, 1.0, 1.0, 1.0, 0.7, 0.5, 0.3, 0.2)

# Dirección del shock por variable (signo del impacto adverso)
direccion <- c(crecimiento   = -1,   # PIB cae
               inflacion     = +1,   # inflación sube
               interbancaria = +1,   # tasa sube
               deprec_fx     = +1,   # peso se deprecia
               embi          = +1)   # spread sube

# Severidad: 3 sigma para adverso moderado, 4.5 sigma para severo
build_crisis_shocks <- function(severity_sigma) {
  t(sapply(seq_len(H), function(h) {
    severity_sigma * patron[h] * direccion * sd_struct
  }))
}

shocks_C_mod <- build_crisis_shocks(3.0)
shocks_C_sev <- build_crisis_shocks(4.5)

# Añadimos ruido residual (la crisis no es determinística)
ruido_mod <- mvrnorm(H, mu = rep(0, K), Sigma = 0.25 * Sigma)
ruido_sev <- mvrnorm(H, mu = rep(0, K), Sigma = 0.25 * Sigma)

C_p50 <- B_p50                                          # baseline = central de B
C_p05 <- base_var + shocks_C_mod + ruido_mod
C_p01 <- base_var + shocks_C_sev + ruido_sev
colnames(C_p05) <- colnames(C_p01) <- variables

# =============================================================================
# DIAGNÓSTICO DE COHERENCIA DEL ENFOQUE A
# =============================================================================

cat("\n=== DIAGNÓSTICO DE COHERENCIA (Enfoque A) ===\n")
cat("¿Cuántas trayectorias simuladas cumplen SIMULTÁNEAMENTE\n")
cat("  crecimiento[h] <= p5_marginal[h]  para todo h ?\n")
hits <- sapply(1:M, function(m) {
  all(sim_paths[, "crecimiento", m] <= A_p05[, "crecimiento"])
})
cat(sprintf("  -> %d de %d trayectorias (%.2f%%)\n",
            sum(hits), M, 100 * mean(hits)))
cat("El 'escenario p5 marginal' NO corresponde a ninguna realidad simulada.\n")

# =============================================================================
# RESUMEN NUMÉRICO POR ENFOQUE (promedio del horizonte)
# =============================================================================

resumen_enfoque <- function(esc_p50, esc_p05, esc_p01, etiqueta) {
  data.frame(
    enfoque   = etiqueta,
    escenario = c("Baseline (p50)", "Adverso (p5)", "Severo (p1)"),
    rbind(colMeans(esc_p50),
          colMeans(esc_p05),
          colMeans(esc_p01))
  )
}

resumen <- bind_rows(
  resumen_enfoque(A_p50, A_p05, A_p01, "A) Marginal"),
  resumen_enfoque(B_p50, B_p05, B_p01, "B) Conjunta"),
  resumen_enfoque(C_p50, C_p05, C_p01, "C) Crisis")
)

cat("\n=== PROMEDIO DEL HORIZONTE POR ENFOQUE Y ESCENARIO ===\n")
print(resumen, row.names = FALSE, digits = 3)

# =============================================================================
# PASO 3: GRÁFICO COMPARATIVO DE LOS TRES ENFOQUES
# =============================================================================

n_hist_plot <- 24
T_total     <- nrow(Y)

# -- Histórico
hist_long <- as.data.frame(Y) |>
  tail(n_hist_plot) |>
  mutate(periodo = (T_total - n_hist_plot + 1):T_total) |>
  pivot_longer(cols = all_of(variables), names_to = "variable",
               values_to = "valor") |>
  mutate(escenario = "Histórico", enfoque = "Histórico")

# Helper: convierte una matriz H x K en long con metadata
mat_to_long <- function(mat, escenario, enfoque) {
  df <- as.data.frame(mat)
  colnames(df) <- variables
  df$periodo   <- (T_total + 1):(T_total + H)
  df |>
    pivot_longer(cols = all_of(variables), names_to = "variable",
                 values_to = "valor") |>
    mutate(escenario = escenario, enfoque = enfoque)
}

esc_long <- bind_rows(
  mat_to_long(A_p50, "Baseline (p50)", "A) Marginal"),
  mat_to_long(A_p05, "Adverso (p5)",   "A) Marginal"),
  mat_to_long(A_p01, "Severo (p1)",    "A) Marginal"),
  mat_to_long(B_p50, "Baseline (p50)", "B) Conjunta"),
  mat_to_long(B_p05, "Adverso (p5)",   "B) Conjunta"),
  mat_to_long(B_p01, "Severo (p1)",    "B) Conjunta"),
  mat_to_long(C_p50, "Baseline (p50)", "C) Crisis"),
  mat_to_long(C_p05, "Adverso (p5)",   "C) Crisis"),
  mat_to_long(C_p01, "Severo (p1)",    "C) Crisis")
)

# Replicamos el histórico en cada panel de enfoque para comparabilidad visual
hist_replicado <- bind_rows(
  hist_long |> mutate(enfoque = "A) Marginal"),
  hist_long |> mutate(enfoque = "B) Conjunta"),
  hist_long |> mutate(enfoque = "C) Crisis")
)

plot_data <- bind_rows(hist_replicado, esc_long) |>
  mutate(escenario = factor(escenario,
                            levels = c("Histórico", "Baseline (p50)",
                                       "Adverso (p5)", "Severo (p1)")))

# Paleta MACROPOL
colores <- c("Histórico"      = "#7F8C8D",
             "Baseline (p50)" = "#1F3B70",
             "Adverso (p5)"   = "#D4A24C",
             "Severo (p1)"    = "#C0392B")

grafico_comparativo <- ggplot(
  plot_data,
  aes(x = periodo, y = valor, color = escenario, linetype = escenario)
) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = T_total, linetype = "dashed",
             color = "grey50", alpha = 0.7) +
  facet_grid(variable ~ enfoque, scales = "free_y", switch = "y") +
  scale_color_manual(values = colores) +
  scale_linetype_manual(values = c("Histórico"      = "solid",
                                   "Baseline (p50)" = "solid",
                                   "Adverso (p5)"   = "dashed",
                                   "Severo (p1)"    = "solid")) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.text       = element_text(face = "bold"),
    strip.placement  = "outside",
    plot.title       = element_text(face = "bold"),
    panel.spacing    = unit(0.8, "lines")
  ) +
  labs(
    title    = "Tres enfoques de escenarios sobre el mismo VAR(2)",
    subtitle = "A: percentiles marginales (incoherente) | B: trayectoria conjunta | C: shocks calibrados a crisis",
    x        = "Periodo",
    y        = NULL,
    color    = "",
    linetype = ""
  )

print(grafico_comparativo)

# =============================================================================
# OPCIONAL: VALORACIÓN DEL PORTAFOLIO BAJO CADA ENFOQUE
# =============================================================================
# Pseudo-modelo de retorno del portafolio AFP en función de las variables macro.
# (Sustituir luego por tu mapeo real M3 -> M4 cuando lo definas.)
#
# Supuesto ilustrativo:
#   ret_AFP_t = 0.02 * crecimiento - 0.30 * inflacion - 0.20 * interbancaria
#               - 0.40 * deprec_fx - 0.05 * embi

beta_port <- c(crecimiento   =  0.02,
               inflacion     = -0.30,
               interbancaria = -0.20,
               deprec_fx     = -0.40,
               embi          = -0.05)

retorno_acum <- function(esc) sum(esc %*% beta_port)

tabla_portafolio <- tibble(
  Escenario = c("Baseline (p50)", "Adverso (p5)", "Severo (p1)"),
  `A) Marginal` = c(retorno_acum(A_p50), retorno_acum(A_p05), retorno_acum(A_p01)),
  `B) Conjunta` = c(retorno_acum(B_p50), retorno_acum(B_p05), retorno_acum(B_p01)),
  `C) Crisis`   = c(retorno_acum(C_p50), retorno_acum(C_p05), retorno_acum(C_p01))
)

cat("\n=== RETORNO ACUMULADO DEL PORTAFOLIO (8 trimestres) ===\n")
print(tabla_portafolio, digits = 3)
cat("\nNota: el coeficiente del portafolio es ilustrativo; reemplazar por\n")
cat("el mapeo real de los pasivos/activos de la AFP en el Módulo 4.\n")
