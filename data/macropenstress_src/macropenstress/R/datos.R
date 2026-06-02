
#' Cargar base macro-previsional del curso
#'
#' Construye las transformaciones usadas en la plantilla original:
#' crecimiento anual del IMAE, inflación anual, depreciación anual, rentabilidad
#' del fondo y salario real. La función usa el CSV incluido en inst/extdata.
#'
#' @param path Ruta opcional al CSV. Si es NULL, usa la base incluida.
#' @return Lista con `datos` crudos y `datos_modelo` transformados.
cargar_datos_pensiones <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "datos_macro_pensiones.csv", package = "macropenstress")
  }
  if (!file.exists(path)) stop("No se encuentra el archivo de datos: ", path)

  datos <- readr::read_csv(path, show_col_types = FALSE)
  if (!"date" %in% names(datos)) stop("La base debe contener la columna `date`.")

  datos <- datos |>
    dplyr::mutate(fecha = lubridate::mdy(.data$date)) |>
    dplyr::arrange(.data$fecha)

  req <- c("imae", "ipc", "tipo_de_cambio", "rentabilidad_afp", "salario_cotizable")
  faltantes <- setdiff(req, names(datos))
  if (length(faltantes) > 0) stop("Faltan columnas requeridas: ", paste(faltantes, collapse = ", "))

  datos_modelo <- datos |>
    dplyr::mutate(
      crecimiento  = 100 * (log(.data$imae) - dplyr::lag(log(.data$imae), 12)),
      inflacion    = 100 * (log(.data$ipc) - dplyr::lag(log(.data$ipc), 12)),
      deprec_fx    = 100 * (log(.data$tipo_de_cambio) - dplyr::lag(log(.data$tipo_de_cambio), 12)),
      ret_fondo    = .data$rentabilidad_afp,
      salario_real = 100 * (log(.data$salario_cotizable / .data$ipc) -
                              dplyr::lag(log(.data$salario_cotizable / .data$ipc), 12))
    ) |>
    tidyr::drop_na()

  return(
    list(
      macro = datos,
      modelo = datos_modelo
    )
  )
}
