###############################################################################
##  sb_etl.R  —  Descarga completa + actualización incremental de SIMBAD
##  ---------------------------------------------------------------------------
##  Construye y mantiene una base SQLite con TODOS los endpoints del API
##  de la Superintendencia de Bancos (RD).
##
##  Modos:
##    "full"        Borra y reconstruye todo desde 'desde' hasta 'hasta'.
##    "incremental" Solo descarga períodos nuevos posteriores al último
##                  registrado en cada tabla. Ideal para correr mensualmente.
##
##  Diseño:
##    * Una tabla por endpoint (indicadores, balance, captaciones_moneda, ...)
##    * Tabla 'sb_log' con auditoría: qué se descargó, cuándo, cuántas filas.
##    * Manejo robusto de fallos: si un endpoint falla, los demás continúan.
##    * Idempotente: correrlo dos veces seguidas no duplica filas
##      (gracias a UNIQUE(periodo, entidad, ...) por endpoint).
##
##  Uso típico:
##    source("sbapi.R")
##    source("sb_etl.R")
##    sb_set_key(Sys.getenv("SB_API_KEY"))
##
##    # Primera vez (descarga histórica completa, ~minutos):
##    sb_etl(db_path = "simbad.sqlite", modo = "full", desde = "2018-01")
##
##    # Cada mes después (rápido):
##    sb_etl(db_path = "simbad.sqlite", modo = "incremental")
###############################################################################

stopifnot(exists("sb_query"))   # requiere sbapi.R cargado antes

# Operador null-coalesce (en caso de que purrr no esté disponible)
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Devuelve un plan con endpoints específicos forzosamente habilitados
#'
#' Útil cuando quieres descargar un endpoint que está deshabilitado por
#' defecto (p.ej. reclamaciones, EIC, subagentes).
#'
#' @examples
#' # Bajar también reclamaciones EIF en una corrida específica
#' sb_etl("simbad.sqlite", modo = "incremental",
#'        plan = sb_etl_activar(c("reclamaciones_eif")))
sb_etl_activar <- function(tablas) {
  lapply(SB_ETL_PLAN, function(item) {
    if (item$tabla %in% tablas) item$enabled <- TRUE
    item
  })
}

# ---- Catálogo de endpoints a descargar -------------------------------------
# Para cada endpoint definimos:
#   tabla        : nombre con que se guardará en SQLite
#   endpoint     : nombre amigable de SB_ENDPOINTS (ver sbapi.R)
#   por_tipo     : si requiere iterar por tipoEntidad (TRUE para EIF; FALSE
#                  para endpoints agregados como tasas o subagentes)
#   tipos        : qué tipos de entidad iterar
#   key_cols     : columnas que forman la llave única para deduplicación
SB_ETL_PLAN <- list(
  
  # === Indicadores ========================================================
  list(tabla = "indicadores_financieros",
       endpoint = "indicadores_financieros",
       por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC","CC","EP"),
       key_cols = c("periodo","entidad")),
  
  list(tabla = "indicadores_principales",
       endpoint = "indicadores_principales",
       por_tipo = FALSE,
       key_cols = c("periodo","indicador")),
  
  list(tabla = "indicadores_riesgo_credito",
       endpoint = "indicadores_riesgo_credito",
       por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad")),
  
  list(tabla = "indicadores_morosidad",
       endpoint = "indicadores_morosidad",
       por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad")),
  
  # === Estados financieros ================================================
  list(tabla = "estado_situacion_eif",
       endpoint = "estado_situacion_eif",
       por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC","CC","EP"),
       key_cols = c("periodo","entidad")),
  
  list(tabla = "estado_resultados_eif",
       endpoint = "estado_resultados_eif",
       por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC","CC","EP"),
       key_cols = c("periodo","entidad")),
  
  list(tabla = "estado_situacion_eic",
       endpoint = "estado_situacion_eic",
       por_tipo = FALSE,
       enabled  = FALSE,   # EIC: requiere parámetros distintos, no aplica a bancos
       key_cols = c("periodo","entidad")),
  
  list(tabla = "estado_resultados_eic",
       endpoint = "estado_resultados_eic",
       por_tipo = FALSE,
       enabled  = FALSE,
       key_cols = c("periodo","entidad")),
  
  # === Cartera ============================================================
  list(tabla = "cartera_riesgo",
       endpoint = "cartera_riesgo",       por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","clasificacionRiesgo")),
  
  list(tabla = "cartera_genero",
       endpoint = "cartera_genero",       por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","genero")),
  
  list(tabla = "cartera_localidad",
       endpoint = "cartera_localidad",    por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","provincia")),
  
  list(tabla = "cartera_moneda",
       endpoint = "cartera_moneda",       por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","moneda")),
  
  list(tabla = "cartera_sector_economico",
       endpoint = "cartera_sector_economico", por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","sectorEconomico")),
  
  list(tabla = "cartera_tipo",
       endpoint = "cartera_tipo",         por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","tipoCartera")),
  
  list(tabla = "cartera_facilidad",
       endpoint = "cartera_facilidad",    por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","tipoFacilidad")),
  
  list(tabla = "cartera_inversiones",
       endpoint = "cartera_inversiones",  por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad")),
  
  # === Captaciones ========================================================
  list(tabla = "captaciones_moneda",
       endpoint = "captaciones_moneda",   por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","moneda")),
  
  list(tabla = "captaciones_localidad",
       endpoint = "captaciones_localidad",por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","provincia")),
  
  list(tabla = "captaciones_sector",
       endpoint = "captaciones_sector",   por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad","sectorDepositante")),
  
  # === Solvencia ==========================================================
  list(tabla = "solvencia",
       endpoint = "solvencia",            por_tipo = TRUE,
       tipos = c("BM","AAyP","BAyC"),
       key_cols = c("periodo","entidad")),
  
  # === Reclamaciones (frecuencia trimestral - deshabilitado por defecto) ===
  list(tabla = "reclamaciones_eif",
       endpoint = "reclamaciones_eif",    por_tipo = FALSE,
       enabled  = FALSE,
       key_cols = c("periodo","entidad")),
  
  list(tabla = "reclamaciones_prousuario",
       endpoint = "reclamaciones_prousuario", por_tipo = FALSE,
       enabled  = FALSE,
       key_cols = c("periodo")),
  
  # === Subagentes y tasas (frecuencia trimestral - deshabilitado) ==========
  list(tabla = "subagentes_operaciones",
       endpoint = "subagentes_operaciones", por_tipo = FALSE,
       enabled  = FALSE,
       key_cols = c("periodo","entidad")),
  
  list(tabla = "subagentes_actividad",
       endpoint = "subagentes_actividad", por_tipo = FALSE,
       enabled  = FALSE,
       key_cols = c("periodo","entidad","actividadEconomica")),
  
  list(tabla = "subagentes_total",
       endpoint = "subagentes_total",     por_tipo = FALSE,
       enabled  = FALSE,
       key_cols = c("periodo")),
  
  list(tabla = "tasas_tarjetas",
       endpoint = "tasas_tarjetas",       por_tipo = FALSE,
       enabled  = FALSE,
       key_cols = c("periodo","entidad"))
)


# ---- Helpers internos ------------------------------------------------------

#' Descarga un endpoint completo para todos los tipos solicitados
.descargar_endpoint <- function(item, desde, hasta) {
  endpoint <- item$endpoint
  
  if (item$por_tipo) {
    out <- purrr::map_dfr(item$tipos, function(t) {
      df <- tryCatch(
        sb_query(endpoint,
                 periodoInicial = desde,
                 periodoFinal   = hasta,
                 tipoEntidad    = t),
        error = function(e) {
          warning(sprintf("  ✗ %s/%s: %s", endpoint, t, conditionMessage(e)))
          data.frame()
        })
      if (nrow(df) > 0) df$tipoEntidadSolicitado <- t
      df
    })
  } else {
    out <- tryCatch(
      sb_query(endpoint,
               periodoInicial = desde,
               periodoFinal   = hasta),
      error = function(e) {
        warning(sprintf("  ✗ %s: %s", endpoint, conditionMessage(e)))
        data.frame()
      })
  }
  out
}

#' Devuelve el último período registrado en una tabla (o NULL si no existe)
.ultimo_periodo <- function(con, tabla) {
  if (!DBI::dbExistsTable(con, tabla)) return(NULL)
  q <- sprintf("SELECT MAX(periodo) AS p FROM %s",
               DBI::dbQuoteIdentifier(con, tabla))
  res <- tryCatch(DBI::dbGetQuery(con, q)$p, error = function(e) NA_character_)
  if (is.null(res) || is.na(res) || res == "") NULL else res
}

#' Suma un mes a un período "YYYY-MM"
.mes_siguiente <- function(periodo) {
  format(seq(lubridate::ym(periodo), by = "month", length.out = 2)[2], "%Y-%m")
}

#' Inserta filas con UPSERT (REPLACE) usando las key_cols como llave única
.upsert <- function(con, tabla, df, key_cols) {
  if (nrow(df) == 0) return(0L)
  
  # Crea la tabla en la primera inserción y agrega índice único
  if (!DBI::dbExistsTable(con, tabla)) {
    DBI::dbWriteTable(con, tabla, df)
    keys_presentes <- key_cols[key_cols %in% names(df)]
    if (length(keys_presentes) > 0) {
      idx_sql <- sprintf(
        "CREATE UNIQUE INDEX IF NOT EXISTS idx_%s ON %s (%s)",
        tabla, tabla,
        paste(DBI::dbQuoteIdentifier(con, keys_presentes), collapse = ",")
      )
      tryCatch(DBI::dbExecute(con, idx_sql),
               error = function(e) {
                 warning(sprintf("No se pudo crear índice en %s: %s",
                                 tabla, conditionMessage(e)))
               })
    }
    return(nrow(df))
  }
  
  # UPSERT manual: borra filas con misma llave, luego inserta
  keys_presentes <- key_cols[key_cols %in% names(df)]
  if (length(keys_presentes) == 0) {
    DBI::dbWriteTable(con, tabla, df, append = TRUE)
    return(nrow(df))
  }
  
  # Eliminar filas existentes que coinciden con las nuevas
  DBI::dbBegin(con)
  on.exit(try(DBI::dbRollback(con), silent = TRUE), add = TRUE)
  
  tmp <- paste0("_tmp_", tabla)
  DBI::dbWriteTable(con, tmp, df[, keys_presentes, drop = FALSE],
                    overwrite = TRUE, temporary = TRUE)
  
  conds <- paste(sprintf("%s.%s = %s.%s",
                         tabla, keys_presentes,
                         tmp, keys_presentes),
                 collapse = " AND ")
  DBI::dbExecute(con, sprintf(
    "DELETE FROM %s WHERE EXISTS (SELECT 1 FROM %s WHERE %s)",
    tabla, tmp, conds))
  DBI::dbExecute(con, sprintf("DROP TABLE %s", tmp))
  
  DBI::dbWriteTable(con, tabla, df, append = TRUE)
  DBI::dbCommit(con)
  on.exit(NULL)
  
  nrow(df)
}

#' Registra una corrida en sb_log
.registrar <- function(con, tabla, modo, desde, hasta, filas, estado, msg = "") {
  if (!DBI::dbExistsTable(con, "sb_log")) {
    DBI::dbExecute(con, "
      CREATE TABLE sb_log (
        ts            TEXT,
        tabla         TEXT,
        modo          TEXT,
        desde         TEXT,
        hasta         TEXT,
        filas         INTEGER,
        estado        TEXT,
        mensaje       TEXT
      )")
  }
  DBI::dbExecute(con,
                 "INSERT INTO sb_log VALUES (?,?,?,?,?,?,?,?)",
                 params = list(as.character(Sys.time()),
                               tabla, modo, desde, hasta,
                               as.integer(filas), estado, substr(msg, 1, 500)))
}


# ---- Función principal -----------------------------------------------------

#' Descarga / actualiza la base SQLite del SIMBAD
#'
#' @param db_path  Ruta del archivo .sqlite.
#' @param modo     "full" (reconstruye) o "incremental" (solo nuevos períodos).
#' @param desde    Período inicial "YYYY-MM" (solo se usa en modo "full" o
#'                 cuando una tabla está vacía en modo "incremental").
#' @param hasta    Período final "YYYY-MM" (default: mes pasado).
#' @param plan     Subconjunto del catálogo a procesar. NULL = todo.
#' @param verbose  Imprime progreso.
#'
#' @return data.frame con resumen de la corrida.
sb_etl <- function(db_path = "simbad.sqlite",
                   modo    = c("incremental", "full"),
                   desde   = "2018-01",
                   hasta   = format(Sys.Date() - 30, "%Y-%m"),
                   plan    = NULL,
                   verbose = TRUE) {
  
  modo <- match.arg(modo)
  if (is.null(plan)) plan <- SB_ETL_PLAN
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  resumen <- data.frame()
  t0_total <- Sys.time()
  
  if (verbose) {
    message(sprintf("\n[%s]  ETL iniciado | modo=%s | hasta=%s | base=%s\n",
                    format(t0_total, "%H:%M:%S"), modo, hasta, db_path))
  }
  
  for (item in plan) {
    # Saltar endpoints deshabilitados (a menos que se incluyan explícitamente
    # vía argumento 'plan')
    if (isFALSE(item$enabled %||% TRUE)) next
    
    t0 <- Sys.time()
    
    # Determinar 'desde' efectivo según modo
    desde_eff <- if (modo == "incremental") {
      ult <- .ultimo_periodo(con, item$tabla)
      if (is.null(ult)) desde else .mes_siguiente(ult)
    } else {
      desde
    }
    
    # Saltar si no hay nada nuevo que pedir
    if (lubridate::ym(desde_eff) > lubridate::ym(hasta)) {
      if (verbose) message(sprintf("  ◦ %s: al día (último=%s)", item$tabla,
                                   .ultimo_periodo(con, item$tabla)))
      .registrar(con, item$tabla, modo, desde_eff, hasta, 0, "skip", "al día")
      next
    }
    
    if (verbose) message(sprintf("  → %s [%s → %s] ...",
                                 item$tabla, desde_eff, hasta))
    
    df <- tryCatch(
      .descargar_endpoint(item, desde_eff, hasta),
      error = function(e) { warning(conditionMessage(e)); data.frame() }
    )
    
    if (nrow(df) == 0) {
      .registrar(con, item$tabla, modo, desde_eff, hasta, 0, "vacio", "")
      if (verbose) message("    (sin datos)")
      next
    }
    
    # Si modo full: borra la tabla antes de escribir
    if (modo == "full" && DBI::dbExistsTable(con, item$tabla)) {
      DBI::dbRemoveTable(con, item$tabla)
    }
    
    n <- tryCatch(
      .upsert(con, item$tabla, df, item$key_cols),
      error = function(e) {
        .registrar(con, item$tabla, modo, desde_eff, hasta, 0,
                   "error", conditionMessage(e))
        warning(sprintf("    ✗ upsert: %s", conditionMessage(e)))
        0L
      })
    
    .registrar(con, item$tabla, modo, desde_eff, hasta, n, "ok")
    if (verbose) {
      dt <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
      message(sprintf("    ✓ %d filas (%ss)", n, dt))
    }
    
    resumen <- rbind(resumen, data.frame(
      tabla = item$tabla, desde = desde_eff,
      hasta = hasta, filas = n,
      stringsAsFactors = FALSE))
  }
  
  # Catálogo de entidades (siempre se reescribe, es estático)
  if (exists("sb_entidades")) {
    DBI::dbWriteTable(con, "entidades", sb_entidades("TODOS"), overwrite = TRUE)
  }
  
  # Metadata global de la base
  meta <- data.frame(
    ultima_actualizacion = as.character(Sys.time()),
    modo                 = modo,
    hasta                = hasta,
    fuente               = "Superintendencia de Bancos RD - SIMBAD API v2",
    base_url             = SB_BASE_URL,
    stringsAsFactors     = FALSE)
  DBI::dbWriteTable(con, "metadata", meta, overwrite = TRUE)
  
  dt_total <- round(as.numeric(Sys.time() - t0_total, units = "mins"), 2)
  if (verbose) {
    message(sprintf("\n[%s]  ETL terminado en %s min | filas totales: %d",
                    format(Sys.time(), "%H:%M:%S"),
                    dt_total, sum(resumen$filas, na.rm = TRUE)))
  }
  
  invisible(resumen)
}


# ---- Helpers de inspección -------------------------------------------------

#' Ver el log de corridas
sb_log <- function(db_path = "simbad.sqlite", n = 50) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM sb_log ORDER BY ts DESC LIMIT %d", n))
}

#' Resumen de cobertura: qué tablas hay, cuántas filas, primer y último período
sb_cobertura <- function(db_path = "simbad.sqlite") {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))
  tablas <- setdiff(DBI::dbListTables(con),
                    c("sb_log", "metadata", "entidades"))
  purrr::map_dfr(tablas, function(t) {
    has_periodo <- "periodo" %in% DBI::dbListFields(con, t)
    if (has_periodo) {
      q <- sprintf("SELECT COUNT(*) n, MIN(periodo) p_min, MAX(periodo) p_max FROM %s",
                   DBI::dbQuoteIdentifier(con, t))
      data.frame(tabla = t, DBI::dbGetQuery(con, q))
    } else {
      n <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) n FROM %s",
                                        DBI::dbQuoteIdentifier(con, t)))$n
      data.frame(tabla = t, n = n, p_min = NA, p_max = NA)
    }
  })
}

###############################################################################
##  USO
###############################################################################
# source("sbapi.R")
# source("sb_etl.R")
# sb_set_key(Sys.getenv("SB_API_KEY"))
#
# # Carga inicial completa (corre una vez, ~varios minutos):
# sb_etl("simbad.sqlite", modo = "full", desde = "2018-01")
#
# # Actualizaciones periódicas (corre mensualmente, ~rápido):
# sb_etl("simbad.sqlite", modo = "incremental")
#
# # Auditoría:
# sb_cobertura("simbad.sqlite")
# sb_log("simbad.sqlite", n = 20)
#
# # Solo un subconjunto (útil para depurar un endpoint problemático):
# sb_etl("simbad.sqlite", modo = "incremental",
#        plan = SB_ETL_PLAN[sapply(SB_ETL_PLAN, \(x) x$tabla == "indicadores_financieros")])
###############################################################################