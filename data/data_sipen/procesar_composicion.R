# ============================================================
# FUNCIÓN MEJORADA: procesar_composicion_afp
# Lee múltiples hojas (meses) y agrega fecha
# ============================================================

procesar_composicion_afp <- function(
    archivo,
    instrumentos = c(
      'Bonos de Hacienda',
      'Certificados de Depósito',
      'Notas BC',
      'Bonos EIF',
      'Bonos Empresas',
      'Valores representativos de deuda emitidos por Fideicomisos de oferta pública',
      'Cuotas de fondos cerrados de inversión',
      'Certificados Inv. Especial BC',
      'Bonos para Financiar Desarrollo de Proyectos de Infraestructura'
    ),
    formato_salida = "long"
) {
  
  # ========================================================
  # 1. VALIDACIÓN E EXTRACCIÓN DE INFORMACIÓN
  # ========================================================
  
  if (!file.exists(archivo)) {
    stop(sprintf("Archivo no existe: %s", archivo))
  }
  
  cat("\n╔════════════════════════════════════════════════════════╗\n")
  cat("║ Procesando:", archivo, "\n")
  cat("╚════════════════════════════════════════════════════════╝\n\n")
  
  # Extraer el año del nombre del archivo
  # Busca patrón: composicion_YYYY.xlsx
  año <- as.numeric(regmatches(archivo, gregexpr("[0-9]{4}", archivo)))
  if (length(año) == 0) {
    stop("No se pudo extraer el año del nombre del archivo. Usa: composicion_YYYY.xlsx")
  }
  año <- año[1]
  
  cat("✓ Año extraído:", año, "\n")
  
  # ========================================================
  # 2. OBTENER NOMBRES DE LAS HOJAS
  # ========================================================
  
  hojas <- readxl::excel_sheets(archivo)
  cat("✓ Hojas encontradas:", length(hojas), "\n")
  cat("  -", paste(hojas, collapse=", "), "\n\n")
  
  # Mapeo de nombres de hojas a números de mes
  nombres_meses <- c(
    "enero" = 1, "enero" = 1,
    "febrero" = 2, "febrero" = 2,
    "marzo" = 3,
    "abril" = 4,
    "mayo" = 5,
    "junio" = 6,
    "julio" = 7,
    "agosto" = 8,
    "setiembre" = 9, "septiembre" = 9,
    "octubre" = 10,
    "noviembre" = 11,
    "diciembre" = 12
  )
  
  # ========================================================
  # 3. PROCESAR CADA HOJA
  # ========================================================
  
  lista_resultados <- list()
  
  for (i in seq_along(hojas)) {
    nombre_hoja <- hojas[i]
    
    cat(sprintf("[%d/%d] Procesando hoja: %s", i, length(hojas), nombre_hoja), " ... ")
    
    # Obtener el número de mes del nombre de la hoja
    nombre_hoja_lower <- tolower(nombre_hoja)
    num_mes <- nombres_meses[names(nombres_meses) == nombre_hoja_lower][1]
    
    # Si no encuentra por nombre, usar la posición
    if (is.na(num_mes)) {
      num_mes <- i
    }
    
    # Validar mes
    if (is.na(num_mes) || num_mes < 1 || num_mes > 12) {
      num_mes <- i
      if (i > 12) {
        cat("⚠️  Omitido (posición fuera de meses)\n")
        next
      }
    }
    
    # Leer la hoja sin nombres
    datos <- readxl::read_excel(archivo, sheet = nombre_hoja, col_names = FALSE)
    
    # Seleccionar columnas: 1, 2, 3, 7, 11, 15, 19, 23, 27
    datos <- datos[, c(1, 2, 3, 7, 11, 15, 19, 23, 27)]
    
    # Asignar nombres temporales
    headers <- paste0("col_", 1:ncol(datos))
    colnames(datos) <- headers
    
    # Guardar la columna 1 (instrumentos)
    col_instrumentos <- as.character(datos[, 1][[1]])
    
    # Filtrar filas con datos válidos
    datos <- datos[!is.na(col_instrumentos) & col_instrumentos != "" & 
                     col_instrumentos != "SUB-SECTOR ECONÓMICO / EMISOR", ]
    col_instrumentos <- col_instrumentos[!is.na(col_instrumentos) & col_instrumentos != "" & 
                                           col_instrumentos != "SUB-SECTOR ECONÓMICO / EMISOR"]
    
    if (nrow(datos) == 0) {
      cat("⚠️  Sin datos válidos\n")
      next
    }
    
    # Remover segunda columna
    datos <- datos[, -2]
    
    # Convertir a numérico
    for (j in 2:ncol(datos)) {
      val <- as.character(datos[[j]])
      val <- gsub("-", "0", val)
      val <- gsub("%|,|\\s", "", val)
      datos[[j]] <- as.numeric(val)
    }
    
    # Filtrar instrumentos
    idx_filtro <- col_instrumentos %in% instrumentos
    datos <- datos[idx_filtro, ]
    col_instrumentos <- col_instrumentos[idx_filtro]
    
    if (nrow(datos) == 0) {
      cat("✓ (sin instrumentos seleccionados)\n")
      next
    }
    
    # Agrupar y sumar por instrumento
    resultado <- data.frame(
      instrumento = unique(col_instrumentos),
      stringsAsFactors = FALSE
    )
    
    for (inst in unique(col_instrumentos)) {
      idx <- col_instrumentos == inst
      temp <- datos[idx, ]
      
      for (j in 2:ncol(datos)) {
        col_name <- paste0("col_", j)
        if (!col_name %in% names(resultado)) {
          resultado[[col_name]] <- 0
        }
        resultado[resultado$instrumento == inst, col_name] <- sum(temp[[j]], na.rm = TRUE)
      }
    }
    
    # Renombrar columnas de fondos
    nombres_fondos <- c("atlantico", "crecer", "jmmb", "popular", "reservas", "romana", "siembra")
    nombres_cols_nuevos <- c("instrumento", nombres_fondos)
    
    if (length(nombres_cols_nuevos) <= ncol(resultado)) {
      colnames(resultado) <- nombres_cols_nuevos[1:ncol(resultado)]
    }
    
    # AGREGAR INFORMACIÓN DE FECHA Y AÑO
    resultado$mes <- num_mes
    resultado$año <- año
    resultado$fecha <- as.Date(sprintf("%d-%02d-01", año, num_mes))
    resultado$nombre_mes <- nombre_hoja
    
    lista_resultados[[i]] <- resultado
    
    cat("✓\n")
  }
  
  # ========================================================
  # 4. COMBINAR TODAS LAS HOJAS
  # ========================================================
  
  if (length(lista_resultados) == 0) {
    stop("No se encontraron datos válidos en ninguna hoja")
  }
  
  resultado_final <- dplyr::bind_rows(lista_resultados)
  
  # ========================================================
  # 5. CONVERTIR A LONG SI SE SOLICITA
  # ========================================================
  
  if (formato_salida == "long") {
    resultado_final <- resultado_final %>%
      tidyr::pivot_longer(
        cols = -c(instrumento, mes, año, fecha, nombre_mes),
        names_to = "fondo",
        values_to = "monto"
      )
  }
  
  cat("\n✓ Completado exitosamente\n")
  cat("  Registros:", nrow(resultado_final), "\n")
  cat("  Período:", sprintf("%d-%02d a %d-%02d", 
                            año, 1, año, 12), "\n\n")
  
  return(resultado_final)
}