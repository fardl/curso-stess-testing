#' Cargar base macro-previsional del curso
#'
#' Construye las transformaciones macro usadas en el curso. La función devuelve
#' únicamente dos data frames:
#' \itemize{
#'   \item `macro_raw`: base original con fecha parseada.
#'   \item `macro`: base limpia para modelación con fecha, inflacion, crecimiento,
#'     interbancaria y deprec_fx.
#' }
#'
#' @param path Ruta opcional al CSV. Si es NULL, usa la base incluida.
#' @return Lista con dos data frames: `macro_raw` y `macro`.
cargar_datos_pensiones <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "datos_macro_pensiones.csv", package = "macropenstress")
  }
  if (!file.exists(path)) stop("No se encuentra el archivo de datos: ", path)

  macro_raw <- readr::read_csv(path, show_col_types = FALSE)
  if (!"date" %in% names(macro_raw)) stop("La base debe contener la columna `date`.")

  macro_raw <- macro_raw |>
    dplyr::mutate(fecha = lubridate::mdy(.data$date)) |>
    dplyr::arrange(.data$fecha)

  req <- c("imae", "ipc", "tipo_de_cambio", "interbancaria")
  faltantes <- setdiff(req, names(macro_raw))
  if (length(faltantes) > 0) {
    stop("Faltan columnas requeridas: ", paste(faltantes, collapse = ", "))
  }

  macro <- macro_raw |>
    dplyr::mutate(
      crecimiento = 100 * (log(.data$imae) - dplyr::lag(log(.data$imae), 12)),
      inflacion   = 100 * (log(.data$ipc) - dplyr::lag(log(.data$ipc), 12)),
      deprec_fx   = 100 * (log(.data$tipo_de_cambio) - dplyr::lag(log(.data$tipo_de_cambio), 12))
    ) |>
    dplyr::select(
      .data$fecha,
      .data$inflacion,
      .data$crecimiento,
      .data$interbancaria,
      .data$deprec_fx
    ) |>
    tidyr::drop_na()

  list(
    macro_raw = macro_raw,
    macro = macro
  )
}
