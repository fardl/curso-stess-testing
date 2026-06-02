###############################################################################
##  sbapi.R  (v2 — rutas corregidas y verificadas)
##  ---------------------------------------------------------------------------
##  Wrapper en R para el API público de la Superintendencia de Bancos (SB)
##  de la República Dominicana — Sistema SIMBAD
##
##  Cambios v2:
##   * Rutas alineadas con el catálogo oficial publicado en el paquete
##     supeRbancos (https://github.com/Lien3105/supeRbancos), verificadas
##     contra apis.sb.gob.do/estadisticas/.
##   * Códigos de tipo de entidad corregidos: AAyP, BAyC (no AAYP/BAC).
##   * URL base con barra final; endpoints SIN barra inicial.
##
##  Portal:  https://desarrollador.sb.gob.do
##  Base:    https://apis.sb.gob.do/estadisticas/
###############################################################################

# ---- 0.  Dependencias ------------------------------------------------------
.required <- c("httr", "jsonlite", "dplyr", "purrr", "DBI",
               "RSQLite", "lubridate", "glue")

.install_if_missing <- function(pkgs) {
  faltan <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(faltan)) install.packages(faltan, repos = "https://cloud.r-project.org")
  invisible(lapply(pkgs, require, character.only = TRUE))
}
.install_if_missing(.required)


# ---- 1.  Configuración global ----------------------------------------------
SB_BASE_URL <- "https://apis.sb.gob.do/estadisticas/v2/"

sb_set_key <- function(key) {
  Sys.setenv(SB_API_KEY = key)
  invisible(TRUE)
}

sb_get_key <- function() {
  k <- Sys.getenv("SB_API_KEY", unset = "")
  if (!nzchar(k)) stop("No se encontró SB_API_KEY. Usa sb_set_key('TU_LLAVE').")
  k
}


# ---- 2.  Catálogo de endpoints (RUTAS VERIFICADAS) -------------------------
# IMPORTANTE: rutas SIN barra inicial; SB_BASE_URL ya termina en "/".
SB_ENDPOINTS <- list(
  
  # --- Captaciones ----------------------------------------------------------
  captaciones_localidad        = "captaciones/localidad",
  captaciones_moneda           = "captaciones/moneda",
  captaciones_sector           = "captaciones/sector-depositante",
  
  # --- Cartera de créditos --------------------------------------------------
  cartera_riesgo               = "carteras/creditos/clasificacion-riesgo",
  cartera_genero               = "carteras/creditos/genero",
  cartera_localidad            = "carteras/creditos/localidad",
  cartera_moneda               = "carteras/creditos/moneda",
  cartera_sector_economico     = "carteras/creditos/sectores-economicos",
  cartera_tipo                 = "carteras/creditos/tipo",
  cartera_facilidad            = "carteras/creditos/facilidad",
  cartera_inversiones          = "carteras/creditos/inversiones",
  
  # --- Cifras de acceso -----------------------------------------------------
  detalle_acceso               = "detalle-entidades/acceso",
  
  # --- Estados financieros --------------------------------------------------
  estado_resultados_eic        = "estados/resultados/eic",
  estado_resultados_eif        = "estados/resultados/eif",
  estado_situacion_eic         = "estados/situacion/eic",
  estado_situacion_eif         = "estados/situacion/eif",
  
  # --- Indicadores ----------------------------------------------------------
  indicadores_morosidad        = "indicadores/morosidad-estresada",
  indicadores_riesgo_credito   = "indicadores/riesgo-credito",
  indicadores_financieros      = "indicadores/financieros",
  indicadores_principales      = "indicadores/principales",
  
  # --- Reclamaciones --------------------------------------------------------
  reclamaciones_eif            = "reclamaciones/eif",
  reclamaciones_prousuario     = "reclamaciones/prousuario",
  
  # --- Solvencia ------------------------------------------------------------
  solvencia                    = "solvencia/componentes",
  
  # --- Subagentes bancarios -------------------------------------------------
  subagentes_operaciones       = "subagentes/operaciones",
  subagentes_actividad         = "subagentes/actividad-economica",
  subagentes_total             = "subagentes/total",
  
  # --- Tasas y comisiones ---------------------------------------------------
  tasas_tarjetas               = "tasas-comisiones/tarjetas-credito"
)

sb_endpoints <- function() {
  data.frame(
    nombre = names(SB_ENDPOINTS),
    ruta   = unlist(SB_ENDPOINTS, use.names = FALSE),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}


# ---- 3.  Tipos de entidad reconocidos --------------------------------------
SB_TIPOS_ENTIDAD <- data.frame(stringsAsFactors = FALSE,
                               codigo      = c("TODOS", "BM", "AAyP", "BAyC", "CC", "EP"),
                               descripcion = c("Sistema Financiero (todas las EIF)",
                                               "Bancos Múltiples",
                                               "Asociaciones de Ahorros y Préstamos",
                                               "Bancos de Ahorro y Crédito",
                                               "Corporaciones de Crédito",
                                               "Entidades Públicas de Intermediación Financiera")
)

sb_tipos_entidad <- function() SB_TIPOS_ENTIDAD


# ---- 4.  Función núcleo de consulta ----------------------------------------
sb_query <- function(endpoint,
                     periodoInicial = NULL,
                     periodoFinal   = NULL,
                     tipoEntidad    = NULL,
                     entidad        = NULL,
                     ...,
                     api_key        = sb_get_key(),
                     raw            = FALSE,
                     verbose        = FALSE,
                     max_retry      = 3) {
  
  if (!is.null(entidad) && !is.null(tipoEntidad)) {
    stop("No se puede pasar 'entidad' y 'tipoEntidad' simultáneamente.")
  }
  
  ruta <- if (endpoint %in% names(SB_ENDPOINTS)) {
    SB_ENDPOINTS[[endpoint]]
  } else {
    sub("^/", "", endpoint)
  }
  
  url <- paste0(SB_BASE_URL, ruta)
  
  query <- list(
    periodoInicial = periodoInicial,
    periodoFinal   = periodoFinal,
    tipoEntidad    = tipoEntidad,
    entidad        = entidad,
    ...
  )
  query <- query[!vapply(query, is.null, logical(1))]
  
  if (verbose) {
    message("GET ", url)
    message("query: ", jsonlite::toJSON(query, auto_unbox = TRUE))
  }
  
  intentos <- 0
  repeat {
    intentos <- intentos + 1
    resp <- tryCatch(
      httr::GET(url,
                httr::add_headers("Ocp-Apim-Subscription-Key" = api_key),
                query  = query,
                encode = "json",
                httr::timeout(60)),
      error = function(e) e
    )
    
    if (inherits(resp, "error")) {
      if (intentos >= max_retry) stop("Error de red: ", conditionMessage(resp))
      Sys.sleep(2 ^ intentos); next
    }
    
    sc <- httr::status_code(resp)
    if (sc == 200) break
    if (sc %in% c(429, 500, 502, 503, 504) && intentos < max_retry) {
      Sys.sleep(2 ^ intentos); next
    }
    
    cuerpo <- httr::content(resp, as = "text", encoding = "UTF-8")
    stop(glue::glue("HTTP {sc} en {ruta}: {substr(cuerpo, 1, 300)}"))
  }
  
  txt    <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- jsonlite::fromJSON(txt, flatten = TRUE)
  
  if (raw) return(parsed)
  
  datos <- if (!is.null(parsed$data)) parsed$data else parsed
  if (is.null(datos) || length(datos) == 0) return(data.frame())
  as.data.frame(datos, stringsAsFactors = FALSE)
}


# ---- 5.  Wrappers de alto nivel --------------------------------------------
# Regla: si el usuario pasa 'entidad', se ignora silenciosamente el default
# de 'tipoEntidad'. La validación dura sigue viva en sb_query() para detectar
# casos en que el usuario pase ambos explícitamente.

sb_indicadores <- function(desde, hasta = NULL,
                           tipoEntidad = "TODOS", entidad = NULL, ...) {
  if (!is.null(entidad)) tipoEntidad <- NULL
  sb_query("indicadores_financieros",
           periodoInicial = desde, periodoFinal = hasta,
           tipoEntidad = tipoEntidad, entidad = entidad, ...)
}

sb_indicadores_sistema <- function(desde, hasta = NULL, ...) {
  sb_query("indicadores_principales",
           periodoInicial = desde, periodoFinal = hasta, ...)
}

sb_balance <- function(desde, hasta = NULL,
                       tipoEntidad = "TODOS", entidad = NULL, ...) {
  if (!is.null(entidad)) tipoEntidad <- NULL
  sb_query("estado_situacion_eif",
           periodoInicial = desde, periodoFinal = hasta,
           tipoEntidad = tipoEntidad, entidad = entidad, ...)
}

sb_resultados <- function(desde, hasta = NULL,
                          tipoEntidad = "TODOS", entidad = NULL, ...) {
  if (!is.null(entidad)) tipoEntidad <- NULL
  sb_query("estado_resultados_eif",
           periodoInicial = desde, periodoFinal = hasta,
           tipoEntidad = tipoEntidad, entidad = entidad, ...)
}

sb_solvencia <- function(desde, hasta = NULL,
                         tipoEntidad = "TODOS", entidad = NULL, ...) {
  if (!is.null(entidad)) tipoEntidad <- NULL
  sb_query("solvencia",
           periodoInicial = desde, periodoFinal = hasta,
           tipoEntidad = tipoEntidad, entidad = entidad, ...)
}

sb_riesgo_credito <- function(desde, hasta = NULL,
                              tipoEntidad = "TODOS", entidad = NULL, ...) {
  if (!is.null(entidad)) tipoEntidad <- NULL
  sb_query("indicadores_riesgo_credito",
           periodoInicial = desde, periodoFinal = hasta,
           tipoEntidad = tipoEntidad, entidad = entidad, ...)
}

sb_morosidad <- function(desde, hasta = NULL,
                         tipoEntidad = "TODOS", entidad = NULL, ...) {
  if (!is.null(entidad)) tipoEntidad <- NULL
  sb_query("indicadores_morosidad",
           periodoInicial = desde, periodoFinal = hasta,
           tipoEntidad = tipoEntidad, entidad = entidad, ...)
}


# ---- 6.  Catálogo de entidades ---------------------------------------------
sb_entidades <- function(tipo = c("TODOS","BM","AAyP","BAyC","CC","EP")) {
  tipo <- match.arg(tipo)
  
  bm <- data.frame(stringsAsFactors = FALSE,
                   sigla  = c("ACTIVO","ADEMI","BANRESERVAS","BDI","BELLBANK","BHD LEON",
                              "CARIBE","CITIBANK","LAFISE","PROMERICA","SANTA CRUZ",
                              "SCOTIABANK","BANCAMERICA","VIMENCA"),
                   nombre = c("Banco Múltiple Activo Dominicana, S.A.",
                              "Banco Múltiple Ademi, S.A.",
                              "Banco de Reservas de la República Dominicana",
                              "Banco Múltiple BDI, S.A.",
                              "Banco Múltiple Bellbank, S.A.",
                              "Banco Múltiple BHD León, S.A.",
                              "Banco Múltiple Caribe Internacional, S.A.",
                              "Citibank, N.A.",
                              "Banco Múltiple Lafise, S.A.",
                              "Banco Múltiple Promérica, S.A.",
                              "Banco Múltiple Santa Cruz, S.A.",
                              "Scotiabank República Dominicana, S.A.",
                              "Banco Múltiple de Las Américas, S.A.",
                              "Banco Múltiple Vimenca, S.A."),
                   tipo   = "BM")
  
  aayp <- data.frame(stringsAsFactors = FALSE,
                     sigla  = c("ALAVER","APAP","DUARTE","LA NACIONAL","ROMANA"),
                     nombre = c("Asociación La Vega Real de Ahorros y Préstamos",
                                "Asociación Popular de Ahorros y Préstamos",
                                "Asociación Duarte de Ahorros y Préstamos",
                                "Asociación La Nacional de Ahorros y Préstamos",
                                "Asociación Romana de Ahorros y Préstamos"),
                     tipo   = "AAyP")
  
  bayc <- data.frame(stringsAsFactors = FALSE,
                     sigla  = c("ADOPEM","ATLANTICO","BANCO BACC","BONANZA","COFACI",
                                "CONFISA","EMPIRE","FIHOGAR","GRUFICORP","JMMB","UNION"),
                     nombre = c("Banco de Ahorro y Crédito Adopem, S.A.",
                                "Banco Atlántico de Ahorro y Crédito, S.A.",
                                "Banco BACC de Ahorro y Crédito del Caribe, S.A.",
                                "Bonanza Banco de Ahorro y Crédito, S.A.",
                                "Banco de Ahorro y Crédito Cofaci, S.A.",
                                "Banco de Ahorro y Crédito Confisa, S.A.",
                                "Banco de Ahorro y Crédito Empire, S.A.",
                                "Banco de Ahorro y Crédito Fihogar, S.A.",
                                "Banco de Ahorro y Crédito Gruficorp, S.A.",
                                "Banco de Ahorro y Crédito JMMB Bank, S.A.",
                                "Banco de Ahorro y Crédito Unión, S.A."),
                     tipo   = "BAyC")
  
  cc <- data.frame(stringsAsFactors = FALSE,
                   sigla  = c("LEASING CONFISA","REIDCO"),
                   nombre = c("Corporación de Crédito Leasing Confisa, S.A.",
                              "Corporación de Crédito Reidco, S.A."),
                   tipo   = "CC")
  
  ep <- data.frame(stringsAsFactors = FALSE,
                   sigla  = "BANCO AGRICOLA",
                   nombre = "Banco Agrícola de la República Dominicana",
                   tipo   = "EP")
  
  todas <- dplyr::bind_rows(bm, aayp, bayc, cc, ep)
  if (tipo == "TODOS") todas else dplyr::filter(todas, tipo == !!tipo)
}


# ---- 7.  Construcción de la base de datos ----------------------------------
sb_construir_bd <- function(db_path     = "simbad.sqlite",
                            desde       = format(Sys.Date() - 730, "%Y-%m"),
                            hasta       = format(Sys.Date() - 30,  "%Y-%m"),
                            tipos       = c("BM", "AAyP", "BAyC"),
                            overwrite   = TRUE,
                            verbose     = TRUE) {
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  bloques <- list(
    indicadores = function(t) sb_indicadores(desde, hasta, tipoEntidad = t),
    balance     = function(t) sb_balance    (desde, hasta, tipoEntidad = t),
    resultados  = function(t) sb_resultados (desde, hasta, tipoEntidad = t),
    solvencia   = function(t) sb_solvencia  (desde, hasta, tipoEntidad = t)
  )
  
  for (nm in names(bloques)) {
    if (verbose) message("→ Descargando ", nm, " ...")
    df <- purrr::map_dfr(tipos, function(t) {
      out <- tryCatch(bloques[[nm]](t), error = function(e) {
        warning(glue::glue("Falló {nm}/{t}: {conditionMessage(e)}")); data.frame()
      })
      if (nrow(out) > 0) out$tipoEntidadSolicitado <- t
      out
    })
    
    if (nrow(df) == 0) {
      if (verbose) message("   (sin datos)"); next
    }
    
    if (overwrite || !DBI::dbExistsTable(con, nm)) {
      DBI::dbWriteTable(con, nm, df, overwrite = TRUE)
    } else {
      DBI::dbWriteTable(con, nm, df, append = TRUE)
    }
    if (verbose) message(glue::glue("   {nrow(df)} filas grabadas en '{nm}'"))
  }
  
  DBI::dbWriteTable(con, "entidades", sb_entidades("TODOS"), overwrite = TRUE)
  
  meta <- data.frame(
    descargado_en = as.character(Sys.time()),
    desde         = desde,
    hasta         = hasta,
    tipos         = paste(tipos, collapse = ","),
    fuente        = "Superintendencia de Bancos RD - SIMBAD API",
    base_url      = SB_BASE_URL
  )
  DBI::dbWriteTable(con, "metadata", meta, overwrite = TRUE)
  
  if (verbose) {
    tablas <- DBI::dbListTables(con)
    message("\n✓ Base lista en: ", normalizePath(db_path))
    message("  Tablas: ", paste(tablas, collapse = ", "))
  }
  invisible(TRUE)
}


# ---- 8.  Utilidades de consulta sobre la BD --------------------------------
sb_load <- function(db_path, tabla) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbReadTable(con, tabla)
}

sb_resumen <- function(db_path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  tablas <- DBI::dbListTables(con)
  purrr::map_dfr(tablas, function(t) {
    n <- DBI::dbGetQuery(con, glue::glue("SELECT COUNT(*) AS n FROM '{t}'"))$n
    data.frame(tabla = t, filas = n)
  })
}

###############################################################################
##  EJEMPLO DE USO
###############################################################################
# sb_set_key("TU_PRIMARY_KEY")
#
# # Indicadores de bancos múltiples 2024
# ind <- sb_indicadores(desde = "2024-01", hasta = "2024-12", tipoEntidad = "BM")
#
# # Construir BD completa últimos 5 años
# sb_construir_bd("simbad.sqlite",
#                 desde = "2020-01", hasta = "2024-12",
#                 tipos = c("BM","AAyP","BAyC"))
#
# sb_resumen("simbad.sqlite")
###############################################################################