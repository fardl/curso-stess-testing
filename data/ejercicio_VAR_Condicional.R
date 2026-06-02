# ============================================================
# ESCENARIOS ADVERSOS CON VAR CONDICIONAL
# ============================================================
rm(list=ls())
library(vars)
library(tidyverse)


# ------------------------------------------------------------
# 1. Datos
# ------------------------------------------------------------

source("plantilla_datos.R")

data_ts <- datos_modelo |>
  dplyr::select(
    embi,
    crecimiento,
    inflacion,
    interbancaria,
    deprec_fx
  ) |>
  as.matrix()

# ------------------------------------------------------------
# 2. Estimar VAR
# ------------------------------------------------------------

lag_select <- VARselect(data_ts, lag.max = 12, type = "const")
p_opt <- lag_select$selection["AIC(n)"]

var_model <- VAR(data_ts, p = p_opt, type = "const")

summary(var_model)

# ------------------------------------------------------------
# 3. Función para forecast condicional
# ------------------------------------------------------------

conditional_VAR_forecast <- function(var_model,
                                     paths_conditioned,
                                     ystart = NULL) {
  
  # Extraer información del VAR
  K <- var_model$K
  p <- var_model$p
  var_names <- colnames(var_model$y)
  B <- Bcoef(var_model)
  
  # Horizonte
  h <- nrow(paths_conditioned)
  
  # Variables condicionadas
  conditioned_vars <- colnames(paths_conditioned)
  
  # Validación
  if (!all(conditioned_vars %in% var_names)) {
    stop("Algunas variables condicionadas no están en el VAR.")
  }
  
  # Historia inicial
  if (is.null(ystart)) {
    yhist <- tail(as.matrix(var_model$y), p)
  } else {
    yhist <- as.matrix(ystart)
  }
  
  colnames(yhist) <- var_names
  
  # Matriz de simulación
  ysim <- rbind(
    yhist,
    matrix(NA, nrow = h, ncol = K)
  )
  
  colnames(ysim) <- var_names
  
  # Simulación recursiva
  for (step in 1:h) {
    
    x <- rep(0, ncol(B))
    names(x) <- colnames(B)
    
    # Rezagos
    for (lag in 1:p) {
      for (v in var_names) {
        cname <- paste0(v, ".l", lag)
        
        if (cname %in% names(x)) {
          x[cname] <- ysim[p + step - lag, v]
        }
      }
    }
    
    # Constante
    if ("const" %in% names(x)) {
      x["const"] <- 1
    }
    
    # Forecast VAR no condicionado
    y_pred <- as.numeric(B %*% x)
    names(y_pred) <- var_names
    
    # Imponer senda condicionada
    y_pred[conditioned_vars] <- as.numeric(paths_conditioned[step, ])
    
    # Guardar resultado
    ysim[p + step, ] <- y_pred
  }
  
  resultado <- as.data.frame(ysim[(p + 1):(p + h), ])
  resultado$trimestre <- 1:h
  
  return(resultado)
}

# ------------------------------------------------------------
# 4. Construir escenario baseline
# ------------------------------------------------------------

h <- 24
variables <- colnames(data_ts)

baseline_paths <- data.frame(
  embi = rep(tail(data_ts[, "embi"], 1), h),
  deprec_fx = rep(2, h)
)

baseline <- conditional_VAR_forecast(
  var_model = var_model,
  paths_conditioned = baseline_paths
)

baseline$escenario <- "Baseline"

# ------------------------------------------------------------
# 5. Construir escenario adverso condicionado
# ------------------------------------------------------------

ultimo_embi <- tail(data_ts[, "embi"], 1)
sigma_fx <- sd(data_ts[, "deprec_fx"], na.rm = TRUE)

adverse_paths <- data.frame(
  embi = ultimo_embi + c(0.5,1,1.5,2,2.5,2,1.5,1,0.5,rep(0.5,15)),
  deprec_fx = c(
    2.0 * sigma_fx,
    2.0 * sigma_fx,
    1.5 * sigma_fx,
    1.5 * sigma_fx,
    1.0 * sigma_fx,
    1.0 * sigma_fx,
    rep(2,18)
  )
)

adverso <- conditional_VAR_forecast(
  var_model = var_model,
  paths_conditioned = adverse_paths
)

adverso$escenario <- "Adverso"

# ------------------------------------------------------------
# 6. Unir escenarios
# ------------------------------------------------------------

escenarios <- bind_rows(
  baseline,
  adverso
)

escenarios_long <- escenarios |>
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "valor"
  )

# ------------------------------------------------------------
# 7. Incorporar histórico
# ------------------------------------------------------------
n_hist_plot <- 50

T_total <- nrow(data_ts)
historico <- as.data.frame(data_ts) |>
  tail(n_hist_plot)

historico$trimestre <- (T_total - n_hist_plot + 1): T_total
historico$escenario <- "Historico"

historico_long <- historico |>
  pivot_longer(
    cols = all_of(variables),
    names_to = "variable",
    values_to = "valor"
  )


escenarios_long <- escenarios_long |>
  mutate(
    trimestre = T_total + trimestre
  )

plot_data <- bind_rows(
  historico_long,
  escenarios_long
)

# ------------------------------------------------------------
# 8. Gráfico histórico + baseline + adverso condicionado
# ------------------------------------------------------------
 ggplot(plot_data,
       aes(x = trimestre, y = valor, color = escenario)) +
  
  geom_line(linewidth = 1) +
  
  geom_vline(
    xintercept = T_total,
    linetype = "dashed"
  ) +
  
  facet_wrap(~ variable, scales = "free_y") +
  
  theme_minimal() +
  
  labs(
    title = paste("Escenario macro con", n_hist_plot, "periodos históricos"),
    subtitle = "Línea punteada = inicio del escenario",
    x = "Tiempo",
    y = "Nivel"
  )

