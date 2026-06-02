# ============================================================
# Taller práctico en R
# Escenario adverso macro-financiero para stress testing
# ============================================================

# 0. Limpieza de sesión ---------------------------------------

rm(list = ls())
graphics.off()

# 1. Carga de paquetes ----------------------------------------

library(macropenstress)
library(vars)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)
library(tidyverse)

# 2. Parámetros generales -------------------------------------

set.seed(123)

h <- 12
M <- 5000



variables_var <- c(
  "crecimiento",
  "inflacion",
  "interbancaria",
  "deprec_fx",
  "embi"
)

# 3. Carga y preparación de datos -----------------------------

datos <- cargar_datos_pensiones()

macro <- datos$macro %>%
  mutate(
    embi = datos$macro_raw$embi[1:n()]
  )
# Fecha de inicio del pronostico
fecha_inicio <- max(macro$fecha) %m+% months(1)

Y <- macro %>%
  dplyr::select(all_of(variables_var)) %>%
  na.omit()

# 4. Estimación del VAR ---------------------------------------

modelo_var <- VAR(
  y = Y,
  p = 2,
  type = "const"
)

roots(modelo_var)

# 5. Escenario baseline ---------------------------------------

baseline <- escenario_var_stress(
  modelo_var = modelo_var,
  h = h,
  M = M,
  enfoques = c("marginal"),
  percentiles = c(0.05, 0.5, 0.95)
)

# 6. Escenario adverso con shock calibrado --------------------

# Escenario adverso macro-financiero:
# - Shock principal: aumento del riesgo país y depreciación cambiaria.
# - Canal de transmisión: mayor inflación y respuesta de tasa.
# - Efecto real: caída del crecimiento.
# - Unidades: desviaciones estándar históricas de cada variable.

shock_adverso <- c(
  crecimiento   = -3.0,
  inflacion     =  10.0,
  interbancaria =  10.0,
  deprec_fx     =  2.0,
  embi          =  5.0
)
adverso <- escenario_var_shock(
  modelo_var = modelo_var,
  h = h,
  tipo = "sigma", 
  shock = shock_adverso,
  fechas = macro$fecha
)

adverso$resultados |>
   filter(escenario=='Adverso shock VAR')
# 7. Escenario conjunto extremo -------------------------------

conjunto_raw <- escenario_var_stress(
  modelo_var = modelo_var,
  h = h,
  M = M,
  enfoques = c("marginal"),
  percentiles = c(0.01, 0.5, 0.99)
)

sim_conjunta <- conjunto_raw$simulaciones
# Construcción del índice de severidad
# La severidad aumenta con:
# - mayor inflación
# - mayor tasa interbancaria
# - mayor depreciación cambiaria
# - mayor EMBI
# - menor crecimiento económico

# Promedio horizonte completo

inflacion_h <- rowMeans(t(sim_conjunta[, "inflacion", ]))
crecimiento_h <- rowMeans(t(sim_conjunta[, "crecimiento", ]))
tasa_h <- rowMeans(t(sim_conjunta[, "interbancaria", ]))
deprec_h <- rowMeans(t(sim_conjunta[, "deprec_fx", ]))
embi_h <- rowMeans(t(sim_conjunta[, "embi", ]))

# Índice de severidad
severity_index <-
  (scale(inflacion_h)) +
  (scale(deprec_h))+
  (scale(tasa_h)) -
  (scale(crecimiento_h))+
  (scale(embi_h))



conjunta <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 5000,
  enfoques = c("conjunta"),
  severity_by_path = severity_index,
  severity_probs = c(0.99),
  conjunta_resumen = "media"
)

# 8. Escenario condicional ------------------------------------

# 8. Escenario condicional ------------------------------------
# Valores en niveles, no en desviaciones estándar.
# Aquí se fija una trayectoria narrativa para las variables clave.

restricciones <- tibble(
  crecimiento = c(-10, -2, 1.5, 1.8, 2.0),
  inflacion = c(8.0, 7.5, 7.0, 6.5, 6.0),
  interbancaria = c(15.0, 10.0, 8.75, 8.5, 8.25),
  deprec_fx = c(8.0, 7.0, 6.0, 5.0, 4.5),
  embi = c(850/100, 725/100, 600/100, 575/100, 550/100)
)

condicional <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 5000,
  enfoques = c("condicional"),
  conditional_paths = list(shock_inflacion = restricciones),
  fill_method = "recursive",
  fechas = macro$fecha, 
  frecuencia = "mensual" 
)


# 9. Consolidación de resultados ------------------------------
fechas_escenario <- seq(
  from = fecha_inicio,
  by = "month",
  length.out = h
)

# -------------------------------------------------------------
# Baseline
# -------------------------------------------------------------

baseline_tbl <- baseline$resultados %>%
  filter(enfoque=='Baseline')%>%
  group_by(variable) %>%
  mutate(
    fecha = fechas_escenario
  ) %>%
  ungroup() %>%
  mutate(
    escenario = "baseline"
  )

# -------------------------------------------------------------
# Adverso
# -------------------------------------------------------------

adverso_tbl <- adverso$resultados %>%
  filter(enfoque!='Baseline') %>%
  group_by(variable) %>%
  mutate(
    fecha = fechas_escenario
  ) %>%
  ungroup() %>%
  mutate(
    escenario = "adverso"
  )

# -------------------------------------------------------------
# Conjunto extremo
# -------------------------------------------------------------

conjunta_tbl <- conjunta$resultados %>%
  filter(enfoque!='Baseline') %>%
  group_by(variable) %>%
  mutate(
    fecha = fechas_escenario
  ) %>%
  ungroup() %>%
  mutate(
    escenario = "conjunta p99"
  )

# -------------------------------------------------------------
# Condicional
# -------------------------------------------------------------

condicional_tbl <- condicional$resultados %>%
  filter(enfoque!='Baseline') %>%
  group_by(variable) %>%
  mutate(
    fecha = fechas_escenario
  ) %>%
  ungroup() %>%
  mutate(
    escenario = "condicional"
  )

# -------------------------------------------------------------
# Consolidar
# -------------------------------------------------------------

escenarios_finales <- bind_rows(
  baseline_tbl,
  adverso_tbl,
  conjunta_tbl,
  condicional_tbl
)

# 10. Exportación ---------------------------------------------

#dir.create("output", showWarnings = FALSE)

#write_csv(
#  escenarios_finales,
#  "output/escenarios_macro_financieros.csv"


# 11. Gráfico -------------------------------------------------

# Número de meses históricos que se desean mostrar
n_hist <- 24

historico_grafico <- macro %>%
  dplyr::select(fecha, all_of(variables_var)) %>%
  na.omit() %>%
  slice_tail(n = n_hist) %>%
  pivot_longer(
    cols = all_of(variables_var),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(
    escenario = "histórico"
  )

escenarios_grafico <- escenarios_finales %>%
  mutate(
    fecha = as.Date(fecha)
  ) %>%
  dplyr::select(fecha, escenario, variable, valor, enfoque)

datos_grafico <- bind_rows(
  historico_grafico,
  escenarios_grafico
) %>%
  mutate(
    escenario = factor(
      escenario,
      levels = c(
        "histórico",
        "baseline",
        "adverso",
        "conjunta p99",
        "condicional"
      )
    )
  )

grafico_escenarios <- ggplot(
  datos_grafico,
  aes(
    x = fecha,
    y = valor,
    group = escenario,
    color = escenario
    #linetype = escenario
  )
) +
  geom_line(linewidth = 0.8) +
  geom_vline(
    xintercept = fecha_inicio,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "Escenarios macro-financieros para stress testing",
    subtitle = "Histórico reciente y trayectorias simuladas a 12 meses",
    x = NULL,
    y = NULL,
    linetype = "Escenario"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

print(grafico_escenarios)



# 12. Tabla resumen: 5 períodos históricos + proyecciones ------

n_hist_tabla <- 5

ultimas_fechas_hist <- datos_grafico %>%
  filter(escenario == "histórico") %>%
  distinct(fecha) %>%
  arrange(desc(fecha)) %>%
  slice_head(n = n_hist_tabla) %>%
  pull(fecha)

tabla_escenarios <- datos_grafico %>%
  filter(
    fecha %in% ultimas_fechas_hist |
      escenario != "histórico"
  ) %>%
  mutate(
    periodo = format(fecha, "%Y-%m")
  ) %>%
  dplyr::select(periodo, escenario, variable, valor) %>%
  arrange(periodo, escenario, variable)

tabla_escenarios_wide <- tabla_escenarios %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  ) %>%
  arrange(periodo, escenario)

print(tabla_escenarios_wide)

write_csv(
  tabla_escenarios_wide,
  "tabla_escenarios_5_hist_mas_proyecciones.csv"
)
# 12. Reproducibilidad ----------------------------------------

sink("output/session_info.txt")
sessionInfo()
sink()
