# Implementacion de un Monte Carlo Simple

# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO VAR: SIMULACION MONTECARLO SIMPLE
# Stress Testing de portafolio de pensiones
# ─────────────────────────────────────────────────────────────────────────────

library(vars)
library(ggplot2)
library(tidyverse)
library(MASS)

source("plantilla_datos.R")

# ─────────────────────────────────────────────────────────────────────────────
# PASO 1: ESTIMAR VAR
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 1: ESTIMACIÓN DEL VAR ===\n")

Y <- datos_modelo |> 
  dplyr::select(crecimiento, inflacion, interbancaria, deprec_fx, embi) |> 
  as.matrix()

# Selección de rezagos
print(VARselect(Y, lag.max = 6, type = "const"))

# Estimar VAR(2)
fit_var <- VAR(Y, p = 2, type = "const")
cat("\nEstabilidad del VAR (raíces deben ser < 1):\n")
print(roots(fit_var))

# pronosticos baseline multivariado
fc_var <- predict(fit_var, n.ahead = 8, ci =0.95)
base_var <- sapply(fc_var$fcst, function(x) x[,"fcst"])
#______________________________________________________________________________
# PASO 2: Funcion iterativa
#______________________________________________________________________________

B <- Bcoef(fit_var)
Sigma <- summary(fit_var)$covres
K <- ncol(Y)
H <- 8
M <- 5000

# Funcion ilustrativa
sim_paths <- array(NA, dim = c(H,K, M))

for (m in 1:M){
  y_lag <- tail(Y,2)
  for (h in 1:H){
    shock <- mvrnorm(1, mu=rep(0,K), Sigma=Sigma)
    y_new <- base_var[h, ] + shock
    sim_paths[h, , m] <- y_new
  }
}

# Percentiles por horizonte y variable
p05 <- apply(sim_paths, c(1,2), quantile, probs=0.05)
p50 <- apply(sim_paths, c(1,2), quantile, probs=0.50)


# ─────────────────────────────────────────────────────────────────────────────
# PASO 3: BLOQUE DE GRÁFICOS CON HISTÓRICO CONTROLABLE
# ─────────────────────────────────────────────────────────────────────────────

# Número de observaciones históricas a mostrar
n_hist_plot <- 24   # Cambiar a 12, 16, 20, 40, etc.

variables <- colnames(Y)
T_total <- nrow(Y)

# ------------------------------------------------------------
# 1. Histórico
# ------------------------------------------------------------

hist_plot <- as.data.frame(Y) |>
  tail(n_hist_plot)

hist_plot$periodo <- (T_total - n_hist_plot + 1):T_total
hist_plot$escenario <- "Histórico"

hist_long <- hist_plot |>
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "valor"
  )

# ------------------------------------------------------------
# 2. Baseline VAR
# ------------------------------------------------------------

base_df <- as.data.frame(base_var)
base_df$periodo <- (T_total + 1):(T_total + H)
base_df$escenario <- "Baseline"

base_long <- base_df |>
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "valor"
  )

# ------------------------------------------------------------
# 3. Percentiles Monte Carlo
# ------------------------------------------------------------

p05_df <- as.data.frame(p05)
colnames(p05_df) <- variables
p05_df$periodo <- (T_total + 1):(T_total + H)
p05_df$escenario <- "Percentil 5%"


p50_df <- as.data.frame(p50)
colnames(p50_df) <- variables
p50_df$periodo <- (T_total + 1):(T_total + H)
p50_df$escenario <- "Mediana Monte Carlo"

p05_long <- p05_df |>
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "valor"
  )

p50_long <- p50_df |>
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "valor"
  )

# ------------------------------------------------------------
# 4. Unir información
# ------------------------------------------------------------

plot_data <- bind_rows(
  hist_long,
  base_long,
  p05_long,
  p50_long
)

# ------------------------------------------------------------
# 5. Gráfico principal
# ------------------------------------------------------------

grafico_mc <- ggplot(
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
    title = "Simulación Monte Carlo VAR: histórico, baseline y escenario adverso",
    subtitle = paste("Histórico mostrado:", n_hist_plot, "observaciones | Línea punteada = inicio del escenario"),
    x = "Periodo",
    y = "Valor",
    color = ""
  )

print(grafico_mc)
