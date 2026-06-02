library(rvest)
library(stringr)
library(dplyr)

# Descargar hojas de excel
extraer_links_con_metadata_real <- function(year) {
  
  library(rvest)
  library(dplyr)
  library(stringr)
  
  base <- "https://www.sipen.gob.do"
  
  url <- paste0(
    base,
    "/estadisticas/fondos-de-pensiones/fondos-de-pensiones-",
    year
  )
  
  page <- read_html(url)
  
  # 🔥 Buscar bloques completos (no solo <a>)
  bloques <- page %>% html_elements("div")
  
  df <- lapply(bloques, function(b) {
    
    texto <- html_text(b, trim = TRUE)
    
    link_node <- html_element(b, "a")
    link <- html_attr(link_node, "href")
    
    if(is.na(link)) return(NULL)
    
    if(!str_detect(link, "\\.xlsx")) return(NULL)
    
    data.frame(
      year = year,
      nombre = texto,
      link = link,
      stringsAsFactors = FALSE
    )
    
  }) %>% bind_rows()
  
  # limpiar links
  df$link <- ifelse(
    str_detect(df$link, "^http"),
    df$link,
    paste0(base, df$link)
  )
  
  return(df)
}


years <- 2017:2026

metadata <- bind_rows(
  lapply(years, extraer_links_con_metadata_real)
)

metadata <- metadata %>%
  mutate(
    variable = case_when(
      str_detect(nombre, "Rentabilidad") ~ "rentabilidad",
      str_detect(nombre, "inter[eé]s") ~ "tasas",
      str_detect(nombre, "composici[oó]n") ~ "portafolio",
      str_detect(nombre, "duraci[oó]n") ~ "duracion",
      TRUE ~ "otros"
    ),
    
    archivo = paste0(variable, "_", year, "_", row_number(), ".xlsx")
  )

descargar_con_metadata <- function(df, carpeta = "data_sipen") {
  
  dir.create(carpeta, showWarnings = FALSE)
  
  for(i in 1:nrow(df)) {
    
    link <- df$link[i]
    destino <- file.path(carpeta, df$archivo[i])
    
    cat("Descargando:", df$archivo[i], "\n")
    
    tryCatch({
      download.file(link, destino, mode = "wb")
    }, error = function(e) {
      cat("Error:", df$archivo[i], "\n")
    })
  }
}

descargar_con_metadata(metadata)


# Cambiar nombres a las hojas de excel


library(readxl)
library(stringr)

clasificar_excel <- function(path) {
  
  # leer solo primeras filas (rápido)
  df <- try(read_excel(path, n_max = 20, col_names = FALSE), silent = TRUE)
  
  if(inherits(df, "try-error")) return("error")
  
  texto <- paste(unlist(df), collapse = " ") %>% tolower()
  
  if(str_detect(texto, "rentabilidad")) {
    return("rentabilidad")
  }
  
  if(str_detect(texto, "duración")) {
    return("duracion")
  }
  
  if(str_detect(texto, "tasa") | str_detect(texto, "interés")) {
    return("tasas")
  }
  
  if(str_detect(texto, "tipo de instrumento") | str_detect(texto, "inversiones")) {
    return("portafolio")
  }
  
  return("otros")
}

library(dplyr)

files <- list.files("data_sipen", full.names = TRUE)

metadata_real <- data.frame(
  archivo = files,
  stringsAsFactors = FALSE
) %>%
  mutate(
    tipo = sapply(archivo, clasificar_excel)
  )


metadata_real <- metadata_real %>%
  mutate(
    year = stringr::str_extract(archivo, "\\d{4}"),
    nuevo_nombre = paste0(tipo, "_", year, "_", row_number(), ".xlsx")
  )


for(i in 1:nrow(metadata_real)) {
  
  old <- metadata_real$archivo[i]
  new <- file.path(dirname(old), metadata_real$nuevo_nombre[i])
  
  if(file.exists(old)) {
    
    ok <- file.rename(old, new)
    
    if(!ok) {
      cat("❌ Error renombrando:", old, "\n")
    } else {
      cat("✅ Renombrado:", metadata_real$nuevo_nombre[i], "\n")
    }
    
  } else {
    cat("⚠️ No existe:", old, "\n")
  }
}


# Extraer informacion de los excels "tasas"
extraer_tasas_pensiones <- function(path, sheet = 1) {
  
  # 1. Leer archivo sin headers
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)
  #raw <- raw %>%
  #  select(c(1, which(raw[8,]=="TIPP")))
  
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




library(readxl)
library(dplyr)
library(stringr)
library(purrr)

# carpeta donde están los excels
folder <- "data_sipen"

files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE)

raw_2017 <- map_dfr()




data_final <- map_dfr(files, function(file) {
  
  cat("Procesando archivo:", basename(file), "\n")
  
  # extraer año desde nombre archivo
  year <- str_extract(basename(file), "\\d{4}")
  
  # obtener nombres de hojas (meses)
  sheets <- excel_sheets(file)
  
  # iterar por cada hoja
  map_dfr(sheets, function(sheet) {
    
    cat("   Hoja:", sheet, "\n")
    
    # intentar procesar
    df <- tryCatch({
      
      extraer_tasas_pensiones(file, sheet = sheet)
      
    }, error = function(e) {
      
      cat("   ❌ Error en:", sheet, "\n")
      return(NULL)
      
    })
    
    if (is.null(df)) return(NULL)
    
    # agregar metadata
    df %>%
      mutate(
        year = as.numeric(year),
        month = sheet,
        file = basename(file)
      )
    
  })
  
})


# Para el anio 2017
# ===============================
# Librerías
# ===============================
library(readxl)
library(dplyr)
library(purrr)

# ===============================
# Ruta del archivo
# ===============================
file_path <- "data_sipen/tasas_2017_1.xlsx"

# ===============================
# Obtener nombres de hojas
# ===============================
sheets <- excel_sheets(file_path)

# ===============================
# Definir el procedimiento
# (MODIFICA ESTA PARTE)
# ===============================
procesar_hoja <- function(data, sheet_name) {
  # 4. Extraer nombres de fondos (fila superior)
  header <- data %>%
    slice(3) %>%
    unlist() %>%
    as.character()
  
  # 5. Limpiar nombres
  header <- str_replace_all(header, "\\s+", "_")
  
  # 6. Asignar nombres
  colnames(data) <- header
  
  # 7. Renombrar primera columna
  colnames(data)[1] <- "instrumento"
  
  
  # 9. Convertir a formato long
  df_long <- data %>%
    slice(-c(1:4, 16:21))%>%
    pivot_longer(
      cols = -instrumento,
      names_to = "fund",
      values_to = "rate"
    )  %>%  
    slice(-(which(.data[["instrumento"]] =="TOTAL"))) %>%
    filter(!is.na(.data[["instrumento"]])) %>%
    mutate(mes = sheet_name)
  
  return(df_long)
}

# ===============================
# Loop funcional sobre hojas
# ===============================
data_final_2017 <- map_df(sheets, function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2017')
  
  return(df_proc)
})

#=======================================
# Para el 2018
#=================================

# ===============================
# Ruta del archivo
# ===============================
file_path <- "data_sipen/tasas_2018_14.xlsx"

sheets <- excel_sheets(file_path)
# ===============================
# Obtener nombres de hojas
# ===============================

# de enero a abril 2018

data_final_2018_01_04 <- map_df(sheets[1:4], function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2018')
  
  return(df_proc)
})

# De mayo a diciembre 2018

procesar_hoja_2018 <- function(data, sheet_name) {
  # 4. Extraer nombres de fondos (fila superior)
  header <- data %>%
    slice(5) %>%
    unlist() %>%
    as.character()
  
  # 2. Limpiar strings vacíos
  header[header == ""] <- NA
  
  # 3. Rellenar hacia la derecha (clave para celdas merged)
  header <- zoo::na.locf(header, na.rm = FALSE)
  # 5. Limpiar nombres
  header <- str_replace_all(header, "\\s+", "_")
  
  # 6. Asignar nombres
  colnames(data) <- header
  
  colnames(data) <- make.unique(header)
  
  # 7. Renombrar primera columna
  colnames(data)[1] <- "instrumento"
  
  
  # 9. Convertir a formato long
  df_long <- data %>%
    select(c(1, which(data[7,]=="TIPP"))) %>%
    slice(-c(1:7, 20:25)) %>%
    pivot_longer(
      cols = -instrumento,
      names_to = "fund",
      values_to = "rate"
    )  %>%  
    slice(-(which(.data[["instrumento"]] =="TOTAL"))) %>%
    filter(!is.na(.data[["instrumento"]])) %>%
    mutate(mes = sheet_name)
  
  return(df_long)
}



# ===============================
# Loop funcional sobre hojas
# ===============================
data_final_2018_05_12 <- map_df(sheets[5:12], function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja_2019(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2018')
  
  return(df_proc)
})




#=======================================
# Para el 2019
#===========================

# ===============================
# Ruta del archivo
# ===============================
file_path <- "data_sipen/tasas_2019_22.xlsx"

# ===============================
# Obtener nombres de hojas
# ===============================
sheets <- excel_sheets(file_path)

procesar_hoja_2019 <- function(data, sheet_name) {
  # 4. Extraer nombres de fondos (fila superior)
  header <- data %>%
    slice(5) %>%
    unlist() %>%
    as.character()
  
  # 2. Limpiar strings vacíos
  header[header == ""] <- NA
  
  # 3. Rellenar hacia la derecha (clave para celdas merged)
  header <- zoo::na.locf(header, na.rm = FALSE)
  # 5. Limpiar nombres
  header <- str_replace_all(header, "\\s+", "_")
  
  # 6. Asignar nombres
  colnames(data) <- header
  
  colnames(data) <- make.unique(header)
  
  # 7. Renombrar primera columna
  colnames(data)[1] <- "instrumento"
  
  
  # 9. Convertir a formato long
  df_long <- data %>%
    select(c(1, which(data[7,]=="TIPP"))) %>%
    slice(-c(1:7, 20:25)) %>%
    pivot_longer(
      cols = -instrumento,
      names_to = "fund",
      values_to = "rate"
    )  %>%  
    slice(-(which(.data[["instrumento"]] =="TOTAL"))) %>%
    filter(!is.na(.data[["instrumento"]])) %>%
    mutate(mes = sheet_name)
  
  return(df_long)
}



# ===============================
# Loop funcional sobre hojas
# ===============================
data_final_2019 <- map_df(sheets, function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja_2019(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2019')
  return(df_proc)
})


#=======================================
# Para el 2020
#===========================

# ===============================
# Ruta del archivo
# ===============================
file_path <- "data_sipen/tasas_2020_101.xlsx"

# ===============================
# Obtener nombres de hojas
# ===============================
sheets <- excel_sheets(file_path)

procesar_hoja_2019 <- function(data, sheet_name) {
  # 4. Extraer nombres de fondos (fila superior)
  header <- data %>%
    slice(5) %>%
    unlist() %>%
    as.character()
  
  # 2. Limpiar strings vacíos
  header[header == ""] <- NA
  
  # 3. Rellenar hacia la derecha (clave para celdas merged)
  header <- zoo::na.locf(header, na.rm = FALSE)
  # 5. Limpiar nombres
  header <- str_replace_all(header, "\\s+", "_")
  
  # 6. Asignar nombres
  colnames(data) <- header
  
  colnames(data) <- make.unique(header)
  
  # 7. Renombrar primera columna
  colnames(data)[1] <- "instrumento"
  
  
  # 9. Convertir a formato long
  df_long <- data %>%
    select(c(1, which(data[7,]=="TIPP"))) %>%
    slice(-c(1:7, 20:25)) %>%
    pivot_longer(
      cols = -instrumento,
      names_to = "fund",
      values_to = "rate"
    )  %>%  
    slice(-(which(.data[["instrumento"]] =="TOTAL"))) %>%
    filter(!is.na(.data[["instrumento"]])) %>%
    mutate(mes = sheet_name)
  
  return(df_long)
}



# ===============================
# Loop funcional sobre hojas
# ===============================
data_final_2020 <- map_df(sheets, function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja_2019(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2020')
  
  return(df_proc)
})



#=======================================
# Para el 2021
#===========================

# ===============================
# Ruta del archivo
# ===============================
file_path <- "data_sipen/tasas_2021_36.xlsx"

# ===============================
# Obtener nombres de hojas
# ===============================
sheets <- excel_sheets(file_path)


procesar_hoja_2021 <- function(data, sheet_name) {
  # 4. Extraer nombres de fondos (fila superior)
  header <- data %>%
    slice(5) %>%
    unlist() %>%
    as.character()
  
  # 2. Limpiar strings vacíos
  header[header == ""] <- NA
  
  # 3. Rellenar hacia la derecha (clave para celdas merged)
  header <- zoo::na.locf(header, na.rm = FALSE)
  # 5. Limpiar nombres
  header <- str_replace_all(header, "\\s+", "_")
  
  # 6. Asignar nombres
  colnames(data) <- header
  
  colnames(data) <- make.unique(header)
  
  # 7. Renombrar primera columna
  colnames(data)[1] <- "instrumento"
  
  
  # 9. Convertir a formato long
  df_long <- data %>%
    select(c(1, which(data[7,]=="TIPP"))) %>%
    slice(-c(1:7, 22:28)) %>%
    pivot_longer(
      cols = -instrumento,
      names_to = "fund",
      values_to = "rate"
    )  %>%  
    slice(-(which(.data[["instrumento"]] =="TOTAL"))) %>%
    filter(!is.na(.data[["instrumento"]])) %>%
    mutate(mes = sheet_name)
  
  return(df_long)
}




# ===============================
# Loop funcional sobre hojas
# ===============================
data_final_2021 <- map_df(sheets, function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja_2021(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2021')
  
  return(df_proc)
})




#=======================================
# Para el 2022
#===========================

# ===============================
# Ruta del archivo
# ===============================
file_path <- "data_sipen/tasas_2022_48.xlsx"

# ===============================
# Obtener nombres de hojas
# ===============================
sheets <- excel_sheets(file_path)



# ===============================
# Loop funcional sobre hojas
# ===============================
data_final_2022 <- map_df(sheets, function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja_2021(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2022')
  
  return(df_proc)
})


#=======================================
# Para el 2023
#===========================

# ===============================
# Ruta del archivo
# ===============================
file_path <- "data_sipen/tasas_2023_58.xlsx"

# ===============================
# Obtener nombres de hojas
# ===============================
sheets <- excel_sheets(file_path)



# ===============================
# Loop funcional sobre hojas
# ===============================
data_final_2023 <- map_df(sheets, function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja_2021(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2023')
  
  return(df_proc)
})


#=======================================
# Para el 2024
#===========================

# ===============================
# Ruta del archivo
# ===============================
file_path <- "data_sipen/tasas_2024_66.xlsx"

# ===============================
# Obtener nombres de hojas
# ===============================
sheets <- excel_sheets(file_path)



# ===============================
# Loop funcional sobre hojas
# ===============================
data_final_2024 <- map_df(sheets, function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja_2021(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2024')
  
  return(df_proc)
})


#=======================================
# Para el 2025
#===========================

# ===============================
# Ruta del archivo
# ===============================
file_path <- "data_sipen/tasas_2025_78.xlsx"

# ===============================
# Obtener nombres de hojas
# ===============================
sheets <- excel_sheets(file_path)



# ===============================
# Loop funcional sobre hojas
# ===============================
data_final_2025 <- map_df(sheets, function(sh) {
  
  # Leer hoja
  df <- read_excel(file_path, sheet = sh)
  
  # Aplicar procedimiento
  df_proc <- procesar_hoja_2021(df, sh)
  df_proc <- df_proc %>%
    mutate(year='2025')
  
  return(df_proc)
})

#---------------------------------------------
data_final <- rbind(data_final_2017, data_final_2018_01_04, data_final_2018_05_12,
                    data_final_2019, data_final_2020, data_final_2021, data_final_2022,
                    data_final_2023, data_final_2024, data_final_2025)
library(stringr)

data_final_ <- data_final %>%
  mutate(
    rate = as.numeric(rate) * 100,
    
    # limpiar fund
    fund = str_replace(fund, "\\.\\d+$", ""),
    
    # limpiar mes (CLAVE)
    mes = str_trim(mes),                 # elimina espacios
    mes = str_replace_all(mes, "\\s+", ""),  # elimina tabs/espacios internos
    
    # transformar mes
    mes = case_when(
      str_detect(mes, "^Ene") ~ "1",
      str_detect(mes, "^Feb") ~ "2",
      str_detect(mes, "^Mar") ~ "3",
      str_detect(mes, "^Abr") ~ "4",
      str_detect(mes, "^May") ~ "5",
      str_detect(mes, "^Jun") ~ "6",
      str_detect(mes, "^Jul") ~ "7",
      str_detect(mes, "^Ago") ~ "8",
      str_detect(mes, "^Sep") ~ "9",
      str_detect(mes, "^Oct") ~ "10",
      str_detect(mes, "^Nov") ~ "11",
      str_detect(mes, "^Dic") ~ "12",
      TRUE ~ mes
    )
  ) %>%
  filter(instrumento %in% c(
    "Bonos EIF 1",
    "Bonos Ministerio de Hacienda",  
    "Certificados de Depósito EIF1",
    "Certificados Inversión Especial BCRD2",
    "Letras BCRD",
    "Letras BCRD2",
    "Notas BCRD2"
  ))


write_csv(data_final_,"rendimientos_por_fondo.csv")
