# ============================================================
# Escenario adverso y muy adverso
# macro-financiero para stress testing
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
rm(list=ls())
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

# ============================================================
# 3. Carga y preparación de datos
# ============================================================
setwd(r'(C:\Users\t490\Documents\MacroPol\stress-testing\data)')
macro <- read.csv("datos_macro.csv")

macro <- macro %>%
  mutate(
    date = mdy(fecha)
  ) %>%
  arrange(date) %>%
  mutate(
    inflacion     = INFLACION,
    deprec_fx     = DTCN,
    crecimiento   = DIMAE,
    embi          = EMBI,
    interbancaria = TASA_PASIVA,
    retorno_afp   = RETORNOS
  ) %>%
  select(-fecha) %>%
  drop_na(
    retorno_afp,
    inflacion,
    deprec_fx,
    crecimiento,
    embi,
    interbancaria
  )

# Fecha de inicio del horizonte
fecha_inicio <- max(macro$date) %m+% months(1)

Y <- macro %>%
  select(all_of(variables_var)) %>%
  na.omit()

# ============================================================
# 4. VAR
# ============================================================

modelo_var <- VAR(
  y = Y,
  p = 2,
  type = "const"
)
plot(irf(modelo_var))
# ============================================================
# 5. Escenarios condicionales
# ============================================================

# Valores en niveles para los primeros meses del horizonte.
# El resto del horizonte se completa recursivamente por el VAR.

restricciones_baseline <- tibble(
  crecimiento   = c(3.0, 4.2, 4.4, 4.5, 5.0),
  inflacion     = c(4.5, 4.3, 4.1, 4.0, 4.0),
  interbancaria = c(8.0, 7.8, 7.5, 7.3, 7.0),
  deprec_fx     = c(2.5, 2.3, 2.0, 2.0, 2.0),
  embi          = c(4.50, 4.40, 4.30, 4.20, 4.10)
)

restricciones_adverso <- tibble(
  crecimiento   = c(-2.0, -1.0, 0.5, 1.0, 1.5),
  inflacion     = c(7.5, 6.8, 6.5, 6.2, 6.0),
  interbancaria = c(11.0, 10.5, 10.0, 9.5, 9.0),
  deprec_fx     = c(10.0, 5.5, 5.0, 4.5, 4.0),
  embi          = c(6.50, 6.25, 6.00, 5.75, 5.50)
)

restricciones_muy_adverso <- tibble(
  crecimiento   = c(-6.0, -4.0, -2.0, -1.0, 0.0),
  inflacion     = c(10.5, 9.5, 9.0, 8.5, 8.0),
  interbancaria = c(15.0, 14.0, 13.0, 12.0, 11.0),
  deprec_fx     = c(20.0, 10.0, 9.0, 8.0, 7.0),
  embi          = c(8.50, 8.00, 7.50, 7.00, 6.50)
)

escenario_condicional <- escenario_var_stress(
  modelo_var = modelo_var,
  h = h,
  M = M,
  enfoques = c("condicional"),
  conditional_paths = list(
    adverso     = restricciones_adverso,
    muy_adverso = restricciones_muy_adverso
  ),
  fill_method = "recursive",
  fechas = macro$date,
  frecuencia = "mensual"
)


# ============================================================
# 6. Consolidación de resultados
# ============================================================

fechas_escenario <- seq(
  from = fecha_inicio,
  by = "month",
  length.out = h
)

escenarios_finales <- escenario_condicional$resultados %>%
  group_by(escenario, variable) %>%
  mutate(
    date = fechas_escenario
  ) %>%
  ungroup() %>%
  mutate(
    escenario = case_when(
      grepl("muy", enfoque, ignore.case = TRUE) ~ "muy_adverso",
      grepl("adverso", enfoque, ignore.case = TRUE) ~ "adverso",
      TRUE ~ escenario
    )
  )

# ============================================================
# 7. Datos para gráfico
# ============================================================

n_hist <- 24

historico_grafico <- macro %>%
  select(date, all_of(variables_var)) %>%
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
    date = as.Date(date)
  ) %>%
  select(
    date,
    escenario,
    variable,
    valor
  )

datos_grafico <- bind_rows(
  historico_grafico,
  escenarios_grafico
) %>%
  arrange(date) %>%
  mutate(
    escenario = factor(
      escenario,
      levels = c(
        "histórico",
        "Baseline VAR",
        "adverso",
        "muy_adverso"
      )
    )
  )

# ============================================================
# 8. Gráfico
# ============================================================

grafico_escenarios <- ggplot(
  datos_grafico,
  aes(
    x = date,
    y = valor,
    color = escenario,
    group = escenario
  )
) +
  geom_line(linewidth = 0.9) +
  geom_vline(
    xintercept = fecha_inicio,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 2
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Escenarios macro-financieros condicionales",
    subtitle = "Baseline, adverso y muy adverso para stress testing previsional",
    x = NULL,
    y = NULL,
    color = "Escenario"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

print(grafico_escenarios)
# 9. Tabla final: 5 períodos históricos + proyecciones ---------

n_hist_tabla <- 5

ultimas_fechas_hist <- datos_grafico %>%
  filter(escenario == "histórico") %>%
  distinct(date) %>%
  arrange(desc(date)) %>%
  slice_head(n = n_hist_tabla) %>%
  pull(date)

tabla_escenarios <- datos_grafico %>%
  filter(
    date %in% ultimas_fechas_hist |
      escenario != "histórico"
  ) %>%
  mutate(
    periodo = format(date, "%Y-%m")
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
  "escenarios.csv"
)
