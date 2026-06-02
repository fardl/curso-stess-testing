#################################################
# script_instalacion_macropenstress.R
# Desinstalar versión anterior e instalar nueva
#################################################

rm(list = ls())
graphics.off()
cat("\014")

cat("=====================================\n")
cat(" Actualizando macropenstress\n")
cat("=====================================\n\n")

# 1. Descargar paquetes cargados
if ("package:macropenstress" %in% search()) {
  detach("package:macropenstress", unload = TRUE, character.only = TRUE)
}

# 2. Desinstalar versión anterior
if ("macropenstress" %in% rownames(installed.packages())) {
  remove.packages("macropenstress")
}

# 3. Instalar dependencias
deps <- c(
  "vars", "forecast", "ggplot2", "dplyr",
  "tidyr", "MASS", "tibble", "zoo"
)

faltan <- deps[!deps %in% rownames(installed.packages())]

if (length(faltan) > 0) {
  install.packages(faltan)
}

# 4. Instalar nueva versión
# IMPORTANTE: el archivo .tar.gz debe estar en la misma carpeta que este script

archivo_pkg <- "macropenstress_0.2.2.tar.gz"

if (!file.exists(archivo_pkg)) {
  stop(
    paste0(
      "No encuentro el archivo ", archivo_pkg, ".\n",
      "Coloca este script y el paquete .tar.gz en la misma carpeta.\n",
      "Carpeta actual: ", getwd()
    )
  )
}

install.packages(
  archivo_pkg,
  repos = NULL,
  type = "source"
)

# 5. Cargar y verificar
library(macropenstress)

cat("\n=====================================\n")
cat(" Instalación completada\n")
cat("=====================================\n\n")

cat("Versión instalada:\n")
print(packageVersion("macropenstress"))

cat("\nFunciones disponibles:\n")
print(ls("package:macropenstress"))

cat("\nEstructura de datos:\n")
datos <- cargar_datos_pensiones()
print(names(datos))

cat("\nVariables en datos$macro:\n")
print(names(datos$macro))

cat("\nListo para iniciar el curso.\n")
