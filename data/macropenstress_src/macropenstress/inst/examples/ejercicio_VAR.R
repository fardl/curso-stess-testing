# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO VAR: ESCENARIO ADVERSO DETERMINISTA
# Stress Testing de portafolio de pensiones
# Una sola trayectoria: shock calibrado en t+1, propagación determinista del VAR
# ─────────────────────────────────────────────────────────────────────────────

library(vars)
library(ggplot2)
library(tidyverse)

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

# ─────────────────────────────────────────────────────────────────────────────
# PASO 2: IRF ANTE SHOCK DE TIPO DE CAMBIO
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 2: IRF ===\n")

irf_fx <- irf(fit_var, 
              impulse = "deprec_fx", 
              response = c("crecimiento", "inflacion", "interbancaria", "embi"),
              n.ahead = 8, 
              boot = TRUE, 
              ci = 0.90)
plot(irf_fx)

# ─────────────────────────────────────────────────────────────────────────────
# FUNCIÓN DE PROYECCIÓN VAR DETERMINISTA
# ─────────────────────────────────────────────────────────────────────────────
# Genera UNA trayectoria del VAR con la opción de inyectar shocks calibrados 
# en t+1. Sin componente aleatorio: shock determinista + propagación pura.
# ─────────────────────────────────────────────────────────────────────────────

project_var <- function(fit, n.ahead, shocks_t1 = NULL) {
  
  # Extraer componentes del VAR
  K <- fit$K                              # número de variables
  p <- fit$p                              # rezagos
  B <- Bcoef(fit)                         # coeficientes
  Y_data <- as.matrix(fit$y)              # datos originales
  var_names <- colnames(Y_data)
  
  # Últimas observaciones para iniciar la proyección
  y_init <- tail(Y_data, p)
  
  # Inicializar trayectoria con los datos históricos
  y_path <- rbind(y_init, matrix(NA, n.ahead, K))
  colnames(y_path) <- var_names
  
  # Loop determinista
  for (h in 1:n.ahead) {
    
    # Construir vector de rezagos (orden: más reciente primero)
    rezagos <- as.vector(t(y_path[(p + h - 1):(h), , drop = FALSE]))
    
    # Vector con constante
    z_t <- c(rezagos, 1)
    
    # Pronóstico determinista: y_t = c + A1*y_{t-1} + ... + Ap*y_{t-p}
    y_new <- B %*% z_t
    
    # Aplicar shock SOLO en t+1 (si fue especificado)
    if (h == 1 && !is.null(shocks_t1)) {
      y_new <- y_new + shocks_t1
    }
    
    y_path[p + h, ] <- as.vector(y_new)
  }
  
  # Devolver solo las observaciones futuras
  return(y_path[(p + 1):(p + n.ahead), , drop = FALSE])
}

# ─────────────────────────────────────────────────────────────────────────────
# PASO 3: CALIBRAR SHOCKS Y GENERAR TRAYECTORIAS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 3: CALIBRACIÓN Y PROYECCIÓN ===\n")

H <- 8  # horizonte

# Calibrar shocks según el slide
sd_vars <- apply(Y, 2, sd, na.rm = TRUE)

shocks_adversos <- c(
  crecimiento   = -2.0 * sd_vars["crecimiento"],   # -2σ
  inflacion     = +1.0 * sd_vars["inflacion"],     # +1σ
  interbancaria = +1.50,                           # +150 pb
  deprec_fx     = +2.0 * sd_vars["deprec_fx"],     # +2σ
  embi          = +2                             # +200 pb
)

cat("Shocks calibrados (aplicados en t+1):\n")
shocks_table <- data.frame(
  Variable = c("PIB (crecimiento)", "Inflación", "Tasa Interbancaria", 
               "Depreciación FX", "Spread (EMBI)"),
  Shock = c("-2σ", "+1σ", "+150 pb", "+2σ", "+200 pb"),
  Valor = round(shocks_adversos, 3),
  Narrativa = c("Desaceleración real", "Presión de precios", 
                "Endurecimiento monetario", "Depreciación", "Riesgo soberano")
)
print(shocks_table)

# Proyectar BASELINE (sin shocks)
cat("\nProyectando trayectoria BASELINE...\n")
baseline_var <- project_var(fit_var, n.ahead = H, shocks_t1 = NULL)

# Proyectar ADVERSO (con shocks calibrados)
cat("Proyectando trayectoria ADVERSA...\n")
adverso_var <- project_var(fit_var, n.ahead = H, shocks_t1 = shocks_adversos)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 4: COMPARACIÓN BASELINE VS ADVERSO
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 4: COMPARACIÓN ===\n")

comparacion_df <- data.frame(
  Trimestre = paste0("t+", 1:H),
  
  PIB_Base = round(baseline_var[, "crecimiento"], 3),
  PIB_Adv = round(adverso_var[, "crecimiento"], 3),
  
  Inf_Base = round(baseline_var[, "inflacion"], 3),
  Inf_Adv = round(adverso_var[, "inflacion"], 3),
  
  Tasa_Base = round(baseline_var[, "interbancaria"], 3),
  Tasa_Adv = round(adverso_var[, "interbancaria"], 3),
  
  FX_Base = round(baseline_var[, "deprec_fx"], 3),
  FX_Adv = round(adverso_var[, "deprec_fx"], 3),
  
  EMBI_Base = round(baseline_var[, "embi"], 3),
  EMBI_Adv = round(adverso_var[, "embi"], 3)
)

print(comparacion_df)

# ─────────────────────────────────────────────────────────────────────────────
# PASO 5: VISUALIZACIÓN
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== VISUALIZACIÓN ===\n")

# Construir fechas
n_hist <- nrow(Y)
fechas_hist <- seq(from = as.Date("2007-10-01"), by = "quarter", length.out = n_hist)
ultima_fecha <- tail(fechas_hist, 1)
fechas_proy <- seq(from = ultima_fecha + 90, by = "quarter", length.out = H)

var_labels <- c("crecimiento" = "PIB (crecimiento)",
                "inflacion" = "Inflación",
                "interbancaria" = "Tasa Interbancaria",
                "deprec_fx" = "Depreciación FX",
                "embi" = "Spread (EMBI)")

# Dataframe de proyecciones
proy_df <- bind_rows(
  data.frame(
    fecha = rep(fechas_proy, 5),
    variable = rep(colnames(Y), each = H),
    valor = as.vector(baseline_var),
    escenario = "Baseline"
  ),
  data.frame(
    fecha = rep(fechas_proy, 5),
    variable = rep(colnames(Y), each = H),
    valor = as.vector(adverso_var),
    escenario = "Adverso"
  )
) |>
  mutate(variable = factor(variable, 
                           levels = c("crecimiento", "inflacion", "interbancaria", 
                                      "deprec_fx", "embi"),
                           labels = var_labels))

# Histórico (últimos 16 trimestres)
hist_df <- as.data.frame(Y) |>
  mutate(fecha = fechas_hist) |>
  pivot_longer(cols = -fecha, names_to = "variable", values_to = "valor") |>
  group_by(variable) |>
  slice_tail(n = 16) |>
  ungroup() |>
  mutate(variable = factor(variable, 
                           levels = c("crecimiento", "inflacion", "interbancaria", 
                                      "deprec_fx", "embi"),
                           labels = var_labels))

# Punto de unión: agregar último histórico al inicio de proyecciones
ultimo_hist <- hist_df |>
  group_by(variable) |>
  slice_tail(n = 1) |>
  ungroup()

union_baseline <- ultimo_hist |> mutate(escenario = "Baseline")
union_adverso <- ultimo_hist |> mutate(escenario = "Adverso")

proy_df_completo <- bind_rows(union_baseline, union_adverso, proy_df) |>
  arrange(escenario, variable, fecha)

# Gráfico
p <- ggplot() +
  # Línea histórica
  geom_line(data = hist_df,
            aes(x = fecha, y = valor),
            color = "#1f77b4", size = 1) +
  # Líneas de proyección
  geom_line(data = proy_df_completo,
            aes(x = fecha, y = valor, color = escenario),
            size = 1) +
  # Separador vertical
  geom_vline(xintercept = ultima_fecha,
             linetype = "dashed", color = "gray50", size = 0.6) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  scale_color_manual(values = c("Baseline" = "#2ca02c", "Adverso" = "#d62728")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  labs(
    title = "VAR Determinista: Histórico + Baseline vs Adverso (8 trimestres)",
    subtitle = "Shock t+1: PIB -2σ | Inflación +1σ | Tasa +150pb | FX +2σ | Spread +200pb",
    x = "Tiempo",
    y = "Valor",
    color = "Escenario"
  )

print(p)
ggsave("M3_VAR_Determinista.png", plot = p, width = 14, height = 9, dpi = 300)
cat("✓ Gráfico guardado: M3_VAR_Determinista.png\n")

# ─────────────────────────────────────────────────────────────────────────────
# PASO 6: FACTORES PARA M4
# ─────────────────────────────────────────────────────────────────────────────

cat("\n=== PASO 6: FACTORES PARA M4 ===\n")

factores_m4 <- data.frame(
  trimestre = 1:H,
  fecha = fechas_proy,
  
  pib_baseline = baseline_var[, "crecimiento"],
  pib_adverso = adverso_var[, "crecimiento"],
  delta_pib = adverso_var[, "crecimiento"] - baseline_var[, "crecimiento"],
  
  inflacion_baseline = baseline_var[, "inflacion"],
  inflacion_adverso = adverso_var[, "inflacion"],
  delta_inflacion = adverso_var[, "inflacion"] - baseline_var[, "inflacion"],
  
  tasa_baseline = baseline_var[, "interbancaria"],
  tasa_adverso = adverso_var[, "interbancaria"],
  delta_tasa = adverso_var[, "interbancaria"] - baseline_var[, "interbancaria"],
  
  fx_baseline = baseline_var[, "deprec_fx"],
  fx_adverso = adverso_var[, "deprec_fx"],
  delta_fx = adverso_var[, "deprec_fx"] - baseline_var[, "deprec_fx"],
  
  spread_baseline = baseline_var[, "embi"],
  spread_adverso = adverso_var[, "embi"],
  delta_spread = adverso_var[, "embi"] - baseline_var[, "embi"]
)

write.csv(factores_m4, "M3_VAR_Determinista_Factores.csv", row.names = FALSE)
cat("✓ Factores exportados: M3_VAR_Determinista_Factores.csv\n")

# Guardar resultados
saveRDS(list(fit_var = fit_var,
             baseline = baseline_var,
             adverso = adverso_var,
             shocks = shocks_adversos),
        "M3_VAR_Determinista.rds")
cat("✓ Resultados guardados: M3_VAR_Determinista.rds\n")

cat("\n", strrep("=", 80), "\n", sep = "")
cat("EJERCICIO COMPLETADO\n")
cat(strrep("=", 80), "\n\n", sep = "")