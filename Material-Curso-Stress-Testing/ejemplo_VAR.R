#################################################
# CURSO:
# Stress Testing en Sistemas de Pensiones
# MACROPOL
#
# EJEMPLO:
# Stress Testing VAR
#################################################
#------------------------------------------------
# 0. Borrar todo
#------------------------------------------------
# 0.1. Limpiar objetos
rm(list=ls())
graphics.off()
cat("\014")

#------------------------------------
# 1. Librerías
#------------------------------------
rm(list=ls())
library(macropenstress)
library(vars)
library(ggplot2)
library(dplyr)

#------------------------------------
# 2. Cargar datos
#------------------------------------

datos <- cargar_datos_pensiones()

macro <- datos$macro

head(macro)

#------------------------------------
# 3. Seleccionar variables macro
#------------------------------------

X <- macro[, c(
  "crecimiento",
  "inflacion",
  "interbancaria",
  "deprec_fx"
)]

X <- na.omit(X)

#------------------------------------
# 4. Estimar VAR
#------------------------------------

modelo_var <- VAR(
  X,
  p = 2,
  type = "const"
)

summary(modelo_var)

#------------------------------------
# 5. Escenario marginal con baseline
#------------------------------------

res_base <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 5000,
  enfoques = c("marginal"),
  percentiles = c(0.05, 0.5, 0.95)
)

#------------------------------------
# 6. Resultados marginal con baseline
#------------------------------------

head(res_base$resultados)

#------------------------------------
# 7. Graficar baseline
#------------------------------------

print(res_base$graficos$panel)

#------------------------------------
# 8. METODO DEL INDICE DE SEVERIDAD
#------------------------------------

sim <- res_base$simulaciones

# Promedio horizonte completo

inflacion_h <- rowMeans(t(sim[, "inflacion", ]))

crecimiento_h <- rowMeans(t(sim[, "crecimiento", ]))

tasa_h <- rowMeans(t(sim[, "interbancaria", ]))

deprec_h <- rowMeans(t(sim[, "deprec_fx", ]))

# Índice de severidad

severity_index <-
  0.25 * as.numeric(scale(inflacion_h)) +
  0.25 * as.numeric(scale(deprec_h))+
  0.25 * as.numeric(scale(tasa_h)) -
  0.25 * as.numeric(scale(crecimiento_h))

summary(severity_index)

#------------------------------------
# 9. Escenario conjunto extremo
#------------------------------------

res_conjunta <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 5000,
  enfoques = c("conjunta"),
  severity_by_path = severity_index,
  severity_probs = c(0.99),
  conjunta_resumen = "media"
)

#------------------------------------
# 10. Graficar escenario conjunto
#------------------------------------

print(res_conjunta$graficos$panel)

#------------------------------------
# 11. Escenario condicional
#------------------------------------

condicion <- data.frame(
  deprec_fx = c(
    10.5, 10.8, 11.0, 11.2
  ),
  crecimiento = c(
    -2, -1,-1,-1
  )
)

res_cond <- escenario_var_stress(
  modelo_var = modelo_var,
  h = 12,
  M = 5000,
  enfoques = c("condicional"),
  conditional_paths = list(shock_inflacion = condicion),
  fill_method = "recursive", #"baseline"
  fechas = macro$fecha, 
  frecuencia = "trimestral" #"mensual", "anual", etc
)


#------------------------------------
# 12. Graficar escenario condicional
#------------------------------------

print(res_cond$graficos$panel)

#------------------------------------
# 13. Exportar gráficos
#------------------------------------

ggsave(
  "VAR_baseline.png",
  res_base$graficos$panel,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  "VAR_conjunta.png",
  res_conjunta$graficos$panel,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  "VAR_condicional.png",
  res_condicional$graficos$panel,
  width = 10,
  height = 7,
  dpi = 300
)

#------------------------------------
# 14. Interpretación económica
#------------------------------------

cat("\n")
cat("=====================================\n")
cat(" INTERPRETACION ECONOMICA\n")
cat("=====================================\n")

cat("
1. Escenario marginal:
   Evalua percentiles individuales
   de cada variable macro.

2. Escenario conjunto:
   Identifica trayectorias extremas
   simultaneas usando indice de
   severidad macro-financiera.

3. Escenario condicional:
   Impone narrativa economica
   especifica y genera trayectorias
   coherentes con el VAR.

Aplicaciones:
- stress testing previsional
- analisis macroprudencial
- ALM
- solvencia
- sostenibilidad del sistema
")





