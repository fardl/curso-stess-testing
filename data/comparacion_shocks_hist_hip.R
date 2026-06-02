# ============================================================
# STRESS TESTING EN PENSIONES
# Escenario histórico vs hipotético
# HORIZONTE: 24 MESES
# ============================================================
rm(list=ls())
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readr)

# ------------------------------------------------------------
# 1. Cargar datos
# ------------------------------------------------------------

datos <- read_csv("datos_macro_pensiones.csv")

# ------------------------------------------------------------
# 2. Preparar fechas
# ------------------------------------------------------------

datos <- datos %>%
  mutate(
    date = mdy(date)
  ) %>%
  arrange(date)

# ------------------------------------------------------------
# 3. Construcción de variables macro mensuales
# ------------------------------------------------------------

datos <- datos %>%
  mutate(
    
    # inflación interanual
    inflacion =
      100 * (ipc / lag(ipc, 12) - 1),
    
    # depreciación interanual
    depreciacion =
      100 * (tipo_de_cambio / lag(tipo_de_cambio, 12) - 1),
    
    # crecimiento actividad
    crecimiento_imae =
      100 * (imae / lag(imae, 12) - 1),
    
    # salario real
    salario_real =
      100 * (
        (salario_cotizable / ipc) /
          lag((salario_cotizable / ipc), 12) - 1
      ),
    
    retorno_afp = rentabilidad_afp
  )

# ------------------------------------------------------------
# 4. Índice mensual de estrés previsional
# ------------------------------------------------------------

datos <- datos %>%
  mutate(
    stress_score =
      0.25 * interbancaria +
      0.20 * inflacion +
      0.20 * depreciacion -
      0.20 * crecimiento_imae -
      0.10 * retorno_afp -
      0.05 * salario_real
  )

# ------------------------------------------------------------
# 5. Escenario histórico
# Peor ventana móvil de 24 meses
# ------------------------------------------------------------

ventana <- 24

datos <- datos %>%
  mutate(
    stress_24m =
      rollapply(
        stress_score,
        width = ventana,
        FUN = mean,
        align = "left",
        fill = NA,
        na.rm = TRUE
      )
  )

inicio_peor <- which.max(datos$stress_24m)

escenario_A <- datos %>%
  slice(inicio_peor:(inicio_peor + ventana - 1)) %>%
  mutate(
    escenario = "Historico",
    h = 1:ventana
  ) %>%
  dplyr::select(
    escenario,
    h,
    date,
    interbancaria,
    inflacion,
    depreciacion,
    crecimiento_imae,
    salario_real,
    retorno_afp
  )

# ------------------------------------------------------------
# 6. Escenario hipotético (24 meses)
# Narrativa:
# tasas altas + depreciación persistente
# ------------------------------------------------------------

ultimo <- tail(datos, 1)

escenario_B <- data.frame(
  
  escenario = "Hipotetico",
  
  h = 1:ventana,
  
  date = seq(
    from = ultimo$date %m+% months(1),
    by = "month",
    length.out = ventana
  ),
  
  interbancaria =
    ultimo$interbancaria +
    c(
      rep(3,6),
      rep(4,6),
      rep(3,6),
      rep(2,6)
    ),
  
  inflacion =
    ultimo$inflacion +
    c(
      rep(2.5,6),
      rep(3,6),
      rep(2,6),
      rep(1,6)
    ),
  
  depreciacion =
    c(
      rep(10,6),
      rep(8,6),
      rep(6,6),
      rep(4,6)
    ),
  
  crecimiento_imae =
    c(
      rep(-4,6),
      rep(-3,6),
      rep(-2,6),
      rep(0,6)
    ),
  
  salario_real =
    c(
      rep(-5,6),
      rep(-4,6),
      rep(-2,6),
      rep(0,6)
    ),
  
  retorno_afp =
    c(
      rep(-8,6),
      rep(-6,6),
      rep(-4,6),
      rep(-2,6)
    )
)

# ------------------------------------------------------------
# 7. Comparación escenarios
# ------------------------------------------------------------

comparacion <- bind_rows(
  escenario_A,
  escenario_B
)

# ------------------------------------------------------------
# 8. Métricas de severidad
# ------------------------------------------------------------

impacto <- comparacion %>%
  group_by(escenario) %>%
  summarise(
    
    tasa_prom =
      mean(interbancaria, na.rm = TRUE),
    
    inflacion_prom =
      mean(inflacion, na.rm = TRUE),
    
    depreciacion_acum =
      sum(depreciacion, na.rm = TRUE),
    
    crecimiento_prom =
      mean(crecimiento_imae, na.rm = TRUE),
    
    perdida_afp =
      -sum(retorno_afp, na.rm = TRUE),
    
    perdida_salario =
      -sum(salario_real, na.rm = TRUE)
  )

print(impacto)

# ------------------------------------------------------------
# 9. Índice agregado de daño previsional
# ------------------------------------------------------------

impacto <- impacto %>%
  mutate(
    
    indice_dano =
      0.40 * perdida_afp +
      0.30 * perdida_salario +
      0.20 * depreciacion_acum -
      0.10 * crecimiento_prom
  )

print(impacto)

# ------------------------------------------------------------
# 10. Escenario recomendado
# ------------------------------------------------------------

reporte <- impacto %>%
  filter(
    indice_dano == max(indice_dano)
  )

cat("\nESCENARIO RECOMENDADO:\n")
print(reporte)

# ------------------------------------------------------------
# 11. Gráficos comparativos
# ------------------------------------------------------------

comparacion_largo <- comparacion %>%
  pivot_longer(
    cols = c(
      interbancaria,
      inflacion,
      depreciacion,
      crecimiento_imae,
      salario_real,
      retorno_afp
    ),
    names_to = "variable",
    values_to = "valor"
  )

ggplot(
  comparacion_largo,
  aes(
    x = h,
    y = valor,
    linetype = escenario
  )
) +
  geom_line(linewidth = 1) +
  facet_wrap(
    ~ variable,
    scales = "free_y"
  ) +
  theme_minimal() +
  labs(
    title = "Stress Testing Mensual: Histórico vs Hipotético",
    x = "Mes del escenario",
    y = "Valor"
  )
