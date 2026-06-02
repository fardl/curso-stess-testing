#################################################
# CURSO:
# Stress Testing en Sistemas de Pensiones
# MACROPOL
#
# Ejercicio 2:
# Construccion de escenario adverso con VAR
#################################################

#------------------------------------------------
# 0. Borrar todo
#------------------------------------------------
# 0.1. Limpiar objetos
rm(list=ls())

# 0.2. Cerrar gráficos
graphics.off()
# 0.3. detach paquetes
pkgs <- names(sessionInfo()$otherPkgs)

if(length(pkgs) > 0){
  for(p in rev(pkgs)){
    try(
      detach(
        paste0("package:", p),
        character.only = TRUE,
        unload = TRUE
      ),
      silent = TRUE
    )
  }
}

# 0.4. Limpiar consola
cat("\014")
gc()



#-----------------------------------------------
# 1. Librerias
#-----------------------------------------------
library(vars)
library(ggplot2)
library(tidyr)
library(dplyr)
library(macropenstress)

#-----------------------------------------------
# 2. Cargar datos
#-----------------------------------------------
datos <- cargar_datos_pensiones()

macro <- datos$macro
# Si solo quieres las primeras 216 observaciones de embi
macro$embi <- datos$macro_raw$embi[1:nrow(macro)]



head(macro)


Y <- macro[, c(
  "embi",
  "crecimiento",
  "inflacion",
  "interbancaria",
  "deprec_fx"
)]

Y <- na.omit(Y)


#-----------------------------------------------
# 3. Seleccion del numero de rezagos del VAR
#-----------------------------------------------
VARselect(Y, lag.max = 4, type="const")

#-----------------------------------------------
# 4. Estimacion 
#-----------------------------------------------
var_estimado <- VAR(Y, p=2, type="const")

#-----------------------------------------------
# 5.Calibracion del escenario adverso (deterministico)
#-----------------------------------------------

shock <- c(
  embi = 2,
  crecimiento = -2,
  inflacion = 1,
  interbancaria = 1.5,
  deprec_fx = 2
)

#-----------------------------------------------
# 6. Simulacion del escenario
#-----------------------------------------------

res_shock <- escenario_var_shock(
  modelo_var = var_estimado,
  shock = shock,
  h = 12,
  tipo = "sigma",
  fechas = macro$fecha,
  frecuencia = "mensual"
)


#-----------------------------------------------
# 7. Grafico de resultados
#-----------------------------------------------
res_shock$graficos$panel

