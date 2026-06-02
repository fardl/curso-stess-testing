#################################################
# CURSO:
# Stress Testing en Sistemas de Pensiones
# MACROPOL
#################################################

cat("=====================================\n")
cat(" Instalando paquete macropenstress\n")
cat("=====================================\n")

#------------------------------------
# 1. Instalar paquetes requeridos
#------------------------------------

paquetes <- c(
  "forecast",
  "vars",
  "ggplot2",
  "dplyr",
  "tidyr",
  "lubridate",
  "MASS",
  "zoo",
  "tseries"
)

instalar <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

if(length(instalar) > 0){
  install.packages(instalar)
}

#------------------------------------
# 2. Instalar paquete del curso
#------------------------------------

install.packages(
  "macropenstress_0.2.2.tar.gz",
  repos = NULL,
  type = "source"
)

#------------------------------------
# 3. Cargar paquete
#------------------------------------

library(macropenstress)

cat("\n")
cat("=====================================\n")
cat(" Paquete instalado correctamente\n")
cat("=====================================\n")

#------------------------------------
# 4. Verificación rápida
#------------------------------------

cat("\nVersion instalada:\n")
print(packageVersion("macropenstress"))

cat("\nFunciones disponibles:\n")
print(ls("package:macropenstress"))