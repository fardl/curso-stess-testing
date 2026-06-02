library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

rm(list=ls())
#path <- "tasa-de-interes-promedio-ponderado-de-las-inversiones-de-los-fondos-de-pensiones-por-tipo-de-instrumento.xlsx"
#sheet <- "Marzo"
extraer_tasas_pensiones <- function(path, sheet = 1) {
  
  # 1. Leer archivo sin headers
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)
  raw <- raw %>%
    select(c(1, which(raw[8,]=="TIPP")))
  
  # 2. Identificar fila donde empieza la tabla
  fila_inicio <- which(str_detect(raw[[1]], "Acciones de oferta pública"))[1]
  
  # 3. Cortar tabla relevante
  df <- raw %>%
    slice(fila_inicio:(fila_inicio + 15))  # ajusta según tabla
  
  # 4. Extraer nombres de fondos (fila superior)
  header <- raw %>%
    slice(fila_inicio - 3) %>%
    unlist() %>%
    as.character()
  
  # 5. Limpiar nombres
  header <- str_replace_all(header, "\\s+", "_")
  
  # 6. Asignar nombres
  colnames(df) <- header
  
  # 7. Renombrar primera columna
  colnames(df)[1] <- "instrumento"
  
  # 8. Filtrar columnas TIPP (tasas)
  
  # 9. Convertir a formato long
  df_long <- df %>%
    pivot_longer(
      cols = -instrumento,
      names_to = "fund",
      values_to = "rate"
    ) %>%
    mutate(
      fund = str_replace(fund, "_TIPP", ""),
      rate = as.numeric(str_replace(rate, "%", "")) / 100
    ) %>%  
    slice(-(which(.data[["instrumento"]] =="TOTAL"))) %>%
    filter(!is.na(.data[["instrumento"]]))
  
  return(df_long)
}


# 2026
df_marzo_2026 <- extraer_tasas_pensiones(
  "tasa-de-interes-promedio-ponderado-de-las-inversiones-de-los-fondos-de-pensiones-por-tipo-de-instrumento.xlsx",
  sheet = "Marzo"
)
df_feb_2026 <- extraer_tasas_pensiones(
  "tasa-de-interes-promedio-ponderado-de-las-inversiones-de-los-fondos-de-pensiones-por-tipo-de-instrumento.xlsx",
  sheet = "Febrero"
)

df_enero_2026 <- extraer_tasas_pensiones(
  "tasa-de-interes-promedio-ponderado-de-las-inversiones-de-los-fondos-de-pensiones-por-tipo-de-instrumento.xlsx",
  sheet = "Enero"
)

