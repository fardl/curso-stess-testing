# =============================================================================
# MACROPOL — Stress Testing en Sistemas de Capitalización Individual
# Módulo 3 — App Shiny: estimación VAR + escenarios determinísticos y Monte Carlo
#
# Cómo correrla:
#   setwd("ruta/a/la/carpeta")
#   shiny::runApp("app.R")
#
# Dependencias:
#   install.packages(c("shiny", "shinydashboard", "DT", "vars", "MASS",
#                      "ggplot2", "tidyr", "dplyr", "readr", "patchwork"))
# =============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(vars)
library(MASS)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(patchwork)

# ─── Paleta MACROPOL ─────────────────────────────────────────────────────────
COLOR_BLUE  <- "#1F3B70"
COLOR_GOLD  <- "#D4A24C"
COLOR_RED   <- "#C0392B"
COLOR_GREY  <- "#7F8C8D"
COLOR_GREEN <- "#639922"

# ─── Datos demo precargados ──────────────────────────────────────────────────
# Generamos un dataset macro dominicano sintético (reproducible) que imita las
# variables del M3 para que la app funcione sin necesidad de cargar nada.
generar_demo <- function() {
  set.seed(20260101)
  n <- 240
  A_true <- matrix(c(
    0.85,  0.05, -0.10,  0.02, -0.05,
    0.10,  0.70,  0.15, -0.05,  0.05,
    -0.05,  0.20,  0.80,  0.05,  0.10,
    0.10, -0.10,  0.05,  0.65,  0.20,
    -0.05,  0.05,  0.10,  0.15,  0.75
  ), 5, 5, byrow = TRUE)
  S_true <- diag(c(1.2, 0.8, 0.5, 1.5, 30)) %*%
    matrix(c(
      1.00, -0.30, -0.20, -0.40, -0.35,
      -0.30,  1.00,  0.50,  0.55,  0.40,
      -0.20,  0.50,  1.00,  0.45,  0.30,
      -0.40,  0.55,  0.45,  1.00,  0.50,
      -0.35,  0.40,  0.30,  0.50,  1.00
    ), 5, 5) %*% diag(c(1.2, 0.8, 0.5, 1.5, 30))
  mu <- c(crecimiento = 4.5, inflacion = 4.0, interbancaria = 6.5,
          deprec_fx = 2.5, embi = 350)
  Y <- matrix(0, n, 5); Y[1, ] <- mu
  for (t in 2:n) {
    e <- mvrnorm(1, mu = c(0,0,0,0,0), Sigma = S_true)
    Y[t, ] <- mu + A_true %*% (Y[t-1, ] - mu) + e
  }
  colnames(Y) <- names(mu)
  fechas <- seq(as.Date("2006-01-01"), by = "quarter", length.out = n)
  df <- as.data.frame(Y); df$fecha <- fechas
  df[, c("fecha", names(mu))]
}

DEMO_DATA <- generar_demo()

# =============================================================================
# UI
# =============================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "MACROPOL — Stress Testing", titleWidth = 320),
  
  dashboardSidebar(
    width = 320,
    sidebarMenu(
      menuItem("1. Datos",       tabName = "datos",       icon = icon("database")),
      menuItem("2. Modelo VAR",  tabName = "modelo",      icon = icon("project-diagram")),
      menuItem("3. Determinístico", tabName = "determ",   icon = icon("ruler")),
      menuItem("4. Monte Carlo", tabName = "montecarlo",  icon = icon("dice")),
      menuItem("5. Comparación", tabName = "comparacion", icon = icon("balance-scale"))
    ),
    hr(),
    div(style = "padding: 8px 16px; color: #ddd; font-size: 11px;",
        "Curso: Stress Testing en Sistemas de", br(),
        "Capitalización Individual", br(),
        "Francisco A. Ramírez de León")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(sprintf("
      .skin-blue .main-header .logo { background-color: %s; font-weight: 600; }
      .skin-blue .main-header .navbar { background-color: %s; }
      .skin-blue .main-sidebar { background-color: #2C3E50; }
      .box.box-solid.box-primary { border-color: %s; }
      .box.box-solid.box-primary > .box-header { background-color: %s; }
      .btn-primary { background-color: %s; border-color: %s; }
      .btn-primary:hover { background-color: #15294F; border-color: #15294F; }
      h2, h3, h4 { color: %s; font-weight: 600; }
      .nota { background-color: #F4F6FA; border-left: 4px solid %s;
              padding: 10px 15px; margin: 10px 0; font-size: 13px; }
    ", COLOR_BLUE, COLOR_BLUE, COLOR_BLUE, COLOR_BLUE,
                                      COLOR_BLUE, COLOR_BLUE, COLOR_BLUE, COLOR_BLUE)))),
    
    tabItems(
      
      # ─── TAB 1: DATOS ────────────────────────────────────────────────────
      tabItem(tabName = "datos",
              h2("Paso 1: Cargar y seleccionar datos"),
              fluidRow(
                box(width = 4, title = "Fuente de datos", status = "primary", solidHeader = TRUE,
                    radioButtons("fuente_datos", NULL,
                                 choices = c("Datos demo (5 variables macro DOM)" = "demo",
                                             "Cargar CSV propio" = "csv"),
                                 selected = "demo"),
                    conditionalPanel(
                      condition = "input.fuente_datos == 'csv'",
                      fileInput("archivo_csv", "Archivo CSV",
                                accept = c(".csv", ".txt"), buttonLabel = "Buscar..."),
                      checkboxInput("csv_header", "Primera fila es encabezado", TRUE),
                      radioButtons("csv_sep", "Separador",
                                   choices = c("Coma" = ",", "Punto y coma" = ";",
                                               "Tabulador" = "\t"), selected = ",", inline = TRUE),
                      div(class = "nota",
                          "El CSV debe tener una columna de fecha y al menos 2 columnas numéricas.")
                    )
                ),
                box(width = 8, title = "Variables a incluir en el modelo",
                    status = "primary", solidHeader = TRUE,
                    uiOutput("selector_vars"),
                    div(class = "nota",
                        "Selecciona entre 2 y 8 variables. Más variables exigen más datos para",
                        " estimar el VAR de manera estable.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Transformaciones por variable",
                    status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    div(class = "nota",
                        strong("Cuándo transformar: "),
                        "Las variables con tendencia (PIB nominal, índices, niveles) suelen",
                        " requerir tasas de crecimiento para ser estacionarias. Las variables",
                        " que ya están en variación (inflación, depreciación) o que son",
                        " spreads (EMBI, tasa interbancaria) usualmente se dejan en nivel."),
                    uiOutput("selector_transforms"),
                    br(),
                    checkboxInput("aplicar_transforms",
                                  "Aplicar transformaciones al modelo VAR", value = FALSE),
                    div(class = "nota", style = "background-color: #FDF3E7; border-left-color: #D4A24C;",
                        strong("Importante: "),
                        "Si activas esta opción, el VAR se estima sobre las variables transformadas.",
                        " Los pronósticos y escenarios estarán también en la escala transformada.",
                        " Recuerda interpretar los resultados en consecuencia.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Vista de los datos", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("diag_fechas"),
                    DTOutput("tabla_datos"),
                    br(),
                    plotOutput("plot_datos", height = "320px")
                )
              )
      ),
      
      # ─── TAB 2: MODELO ───────────────────────────────────────────────────
      tabItem(tabName = "modelo",
              h2("Paso 2: Estimar el VAR"),
              fluidRow(
                box(width = 4, title = "Especificación", status = "primary", solidHeader = TRUE,
                    sliderInput("p_lags", "Rezagos (p):",
                                min = 1, max = 8, value = 2, step = 1),
                    radioButtons("var_tipo", "Términos determinísticos:",
                                 choices = c("Constante" = "const",
                                             "Constante + tendencia" = "both",
                                             "Ninguno" = "none"),
                                 selected = "const"),
                    actionButton("btn_estimar", "Estimar VAR",
                                 icon = icon("play"),
                                 class = "btn-primary",
                                 style = "width: 100%;"),
                    br(), br(),
                    div(class = "nota",
                        "El criterio AIC/BIC se calcula automáticamente para p = 1..8.")
                ),
                box(width = 8, title = "Selección automática de rezagos",
                    status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("varselect_out"),
                    div(class = "nota",
                        "AIC tiende a sobreparametrizar; SC (BIC) es más parsimonioso.",
                        " Para stress testing se recomienda revisar ambos.")
                )
              ),
              fluidRow(
                box(width = 6, title = "Estabilidad (raíces)", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("var_roots")
                ),
                box(width = 6, title = "Pronóstico baseline", status = "primary", solidHeader = TRUE,
                    sliderInput("horizonte", "Horizontes a proyectar (H):",
                                min = 4, max = 24, value = 8, step = 1),
                    verbatimTextOutput("base_var_summary")
                )
              )
      ),
      
      # ─── TAB 3: DETERMINÍSTICO ───────────────────────────────────────────
      tabItem(tabName = "determ",
              h2("Paso 3: Enfoque determinístico"),
              fluidRow(
                box(width = 4, title = "Modo determinístico", status = "primary", solidHeader = TRUE,
                    radioButtons("modo_determ", NULL,
                                 choices = c("Pronóstico puntual del VAR (sin shocks)" = "puntual",
                                             "Pronóstico + shocks calibrados manualmente" = "shocks"),
                                 selected = "puntual"),
                    conditionalPanel(
                      condition = "input.modo_determ == 'shocks'",
                      hr(),
                      h4("Calibración de shocks"),
                      p("Magnitud del shock por variable, en desviaciones estándar de los residuos.",
                        style = "font-size: 12px; color: #666;"),
                      uiOutput("shock_sliders"),
                      hr(),
                      sliderInput("persistencia", "Persistencia (trimestres a 100%):",
                                  min = 1, max = 8, value = 4, step = 1),
                      div(class = "nota",
                          "Los shocks se aplican al 100% durante los primeros N trimestres",
                          " y se atenúan linealmente después.")
                    ),
                    br(),
                    actionButton("btn_determ", "Calcular escenario determinístico",
                                 icon = icon("calculator"),
                                 class = "btn-primary",
                                 style = "width: 100%;")
                ),
                box(width = 8, title = "Escenario determinístico",
                    status = "primary", solidHeader = TRUE,
                    plotOutput("plot_determ", height = "550px")
                )
              ),
              fluidRow(
                box(width = 12, title = "Trayectoria proyectada", status = "primary", solidHeader = TRUE,
                    DTOutput("tabla_determ"),
                    br(),
                    downloadButton("dl_determ", "Descargar CSV", class = "btn-primary")
                )
              )
      ),
      
      # ─── TAB 4: MONTE CARLO ──────────────────────────────────────────────
      tabItem(tabName = "montecarlo",
              h2("Paso 4: Enfoques Monte Carlo"),
              fluidRow(
                box(width = 4, title = "Configuración Monte Carlo",
                    status = "primary", solidHeader = TRUE,
                    sliderInput("M_sim", "Número de simulaciones:",
                                min = 500, max = 10000, value = 5000, step = 500),
                    numericInput("semilla", "Semilla (set.seed):",
                                 value = 20260501, min = 1, max = 1e9),
                    radioButtons("motor_mc", "Motor de simulación:",
                                 choices = c("Normal con Cholesky (mvrnorm)" = "normal",
                                             "Bootstrap residual" = "bootstrap"),
                                 selected = "normal"),
                    hr(),
                    sliderInput("alpha_adv", "Percentil adverso (%):",
                                min = 1, max = 25, value = 5, step = 1),
                    sliderInput("alpha_sev", "Percentil severo (%):",
                                min = 0.5, max = 10, value = 1, step = 0.5),
                    hr(),
                    h4("Calibración del Enfoque C"),
                    sliderInput("crisis_sigma", "Severidad (σ del shock):",
                                min = 1, max = 5, value = 3, step = 0.5),
                    sliderInput("crisis_persist", "Persistencia crisis (trimestres):",
                                min = 2, max = 8, value = 4, step = 1),
                    br(),
                    actionButton("btn_mc", "Ejecutar Monte Carlo",
                                 icon = icon("play-circle"),
                                 class = "btn-primary",
                                 style = "width: 100%;")
                ),
                box(width = 8, title = "Distribución de simulaciones",
                    status = "primary", solidHeader = TRUE,
                    radioButtons("vista_mc", NULL,
                                 choices = c("Enfoque A — Marginal" = "A",
                                             "Enfoque B — Conjunto" = "B",
                                             "Enfoque C — Crisis"   = "C"),
                                 selected = "A", inline = TRUE),
                    plotOutput("plot_mc", height = "500px")
                )
              ),
              fluidRow(
                box(width = 12, title = "Diagnóstico", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("mc_diagnostico")
                )
              )
      ),
      
      # ─── TAB 5: COMPARACIÓN ──────────────────────────────────────────────
      tabItem(tabName = "comparacion",
              h2("Paso 5: Comparación lado a lado"),
              fluidRow(
                box(width = 12, title = "Los tres enfoques sobre el mismo VAR",
                    status = "primary", solidHeader = TRUE,
                    uiOutput("selector_var_comp"),
                    plotOutput("plot_comparacion", height = "550px"),
                    div(class = "nota",
                        "Mismo VAR, mismas simulaciones, tres estrategias para extraer escenarios.",
                        " La diferencia entre A y C revela cuánta cola adicional aporta la calibración",
                        " explícita de crisis frente a los percentiles puramente estadísticos.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Descargar resultados", status = "primary", solidHeader = TRUE,
                    p("Descarga el escenario combinado (baseline + adverso + severo de cada enfoque)",
                      " en CSV para análisis posterior."),
                    downloadButton("dl_comparacion", "Descargar comparación CSV",
                                   class = "btn-primary")
                )
              )
      )
      
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  # ─── 1. CARGA DE DATOS ─────────────────────────────────────────────────────
  
  # Parser de fechas robusto: prueba múltiples formatos comunes
  parsear_fecha <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (is.numeric(x)) {
      # Si es numérico, podría ser un serial de Excel o un año
      if (all(x > 1900 & x < 2100, na.rm = TRUE)) {
        # Parece ser años: convertir a fechas trimestrales del primer trimestre
        return(as.Date(paste0(floor(x), "-01-01")))
      }
      # Serial de Excel (días desde 1899-12-30)
      return(as.Date(x, origin = "1899-12-30"))
    }
    
    x_char <- as.character(x)
    x_char <- trimws(x_char)
    
    # Casos especiales: trimestres como 2003Q1, 2003-Q1, 2003q1
    if (any(grepl("[Qq]", x_char))) {
      partes <- regmatches(x_char,
                           regexec("([0-9]{4})[^0-9]*[Qq]([1-4])", x_char))
      fechas <- sapply(partes, function(m) {
        if (length(m) == 3) {
          year <- as.integer(m[2])
          q <- as.integer(m[3])
          mes <- (q - 1) * 3 + 1
          return(sprintf("%04d-%02d-01", year, mes))
        }
        return(NA_character_)
      })
      out <- as.Date(fechas)
      if (sum(!is.na(out)) > length(out) * 0.8) return(out)
    }
    
    # Casos especiales: año-mes como 2003M1, 2003-M01, 2003m01
    if (any(grepl("[Mm]", x_char))) {
      partes <- regmatches(x_char,
                           regexec("([0-9]{4})[^0-9]*[Mm]([0-9]{1,2})", x_char))
      fechas <- sapply(partes, function(m) {
        if (length(m) == 3) {
          return(sprintf("%04d-%02d-01", as.integer(m[2]), as.integer(m[3])))
        }
        return(NA_character_)
      })
      out <- as.Date(fechas)
      if (sum(!is.na(out)) > length(out) * 0.8) return(out)
    }
    
    # Probar formatos estándar en orden de preferencia
    formatos <- c(
      "%Y-%m-%d", "%Y/%m/%d",
      "%d/%m/%Y", "%d-%m-%Y",
      "%m/%d/%Y", "%m-%d-%Y",
      "%Y-%m", "%Y/%m",
      "%m/%Y", "%m-%Y",
      "%Y"
    )
    for (fmt in formatos) {
      out <- suppressWarnings(as.Date(x_char, format = fmt))
      if (sum(!is.na(out)) > length(out) * 0.8) return(out)
    }
    
    # Si nada funciona, devolver NA
    rep(as.Date(NA), length(x_char))
  }
  
  datos_raw <- reactive({
    if (input$fuente_datos == "demo") {
      DEMO_DATA
    } else {
      req(input$archivo_csv)
      df <- read.csv(input$archivo_csv$datapath,
                     header = input$csv_header,
                     sep = input$csv_sep,
                     stringsAsFactors = FALSE)
      
      # Intentar parsear primera columna como fecha de forma robusta
      fechas_parseadas <- parsear_fecha(df[[1]])
      
      if (sum(!is.na(fechas_parseadas)) > length(fechas_parseadas) * 0.8) {
        df[[1]] <- fechas_parseadas
        names(df)[1] <- "fecha"
      } else {
        # Si no se puede parsear, generar índice secuencial trimestral
        showNotification(
          paste("No se pudo identificar el formato de fecha en la primera columna.",
                "Se generará un índice trimestral artificial. Verifica el CSV."),
          type = "warning", duration = 8)
        df[[1]] <- seq(as.Date("2000-01-01"), by = "quarter", length.out = nrow(df))
        names(df)[1] <- "fecha"
      }
      
      # Eliminar filas con fecha NA
      df <- df[!is.na(df[[1]]), , drop = FALSE]
      df
    }
  })
  
  output$selector_vars <- renderUI({
    df <- datos_raw()
    vars_num <- names(df)[sapply(df, is.numeric)]
    checkboxGroupInput("vars_seleccionadas", NULL,
                       choices = vars_num,
                       selected = vars_num[1:min(5, length(vars_num))],
                       inline = TRUE)
  })
  
  # Selector de transformación por variable seleccionada
  output$selector_transforms <- renderUI({
    vars <- input$vars_seleccionadas
    if (is.null(vars) || length(vars) == 0) {
      return(div(class = "nota",
                 "Selecciona variables arriba para configurar transformaciones."))
    }
    # Sugerencias por defecto basadas en nombres conocidos del curso
    sugerencia <- function(v) {
      v_low <- tolower(v)
      if (grepl("pib|gdp|indice|index|monetar|m1|m2|credit|nivel|valor",
                v_low)) return("log_diff")
      if (grepl("inflacion|inflation|deprec|growth|crecim|var_|d_", v_low))
        return("none")
      "none"
    }
    fluidRow(
      lapply(vars, function(v) {
        column(width = 4,
               selectInput(paste0("trans_", v), label = v,
                           choices = c("Sin transformar (nivel)" = "none",
                                       "Tasa de crecimiento (Δ% trimestral)" = "pct_change",
                                       "Log-diferencia (≈ tasa continua)"     = "log_diff",
                                       "Diferencia simple (Yt − Yt−1)"        = "diff",
                                       "Tasa interanual (Δ% 4 trimestres)"    = "yoy"),
                           selected = sugerencia(v))
        )
      })
    )
  })
  
  # Aplicar transformación a una serie según el método elegido
  aplicar_transformacion <- function(x, metodo) {
    switch(metodo,
           "none"       = x,
           "pct_change" = c(NA, 100 * (x[-1] / x[-length(x)] - 1)),
           "log_diff"   = c(NA, 100 * diff(log(x))),
           "diff"       = c(NA, diff(x)),
           "yoy"        = c(rep(NA, 4), 100 * (x[-(1:4)] / x[1:(length(x)-4)] - 1)),
           x
    )
  }
  
  datos_modelo <- reactive({
    df <- datos_raw()
    req(input$vars_seleccionadas)
    validate(need(length(input$vars_seleccionadas) >= 2,
                  "Selecciona al menos 2 variables."))
    validate(need(length(input$vars_seleccionadas) <= 8,
                  "Selecciona como máximo 8 variables."))
    
    df_out <- df[, c(names(df)[1], input$vars_seleccionadas), drop = FALSE]
    
    # Ordenar por fecha (esencial para que las transformaciones tengan sentido)
    if (inherits(df_out[[1]], "Date")) {
      df_out <- df_out[order(df_out[[1]]), , drop = FALSE]
    }
    
    # Aplicar transformaciones si está activado
    if (isTRUE(input$aplicar_transforms)) {
      for (v in input$vars_seleccionadas) {
        metodo <- input[[paste0("trans_", v)]]
        if (!is.null(metodo) && metodo != "none") {
          x <- df_out[[v]]
          # Validar que log_diff/pct_change no reciban valores no positivos
          if (metodo %in% c("log_diff", "pct_change", "yoy") &&
              any(x <= 0, na.rm = TRUE)) {
            showNotification(
              sprintf("La variable '%s' contiene valores ≤ 0; '%s' no es válida. Se mantiene en nivel.",
                      v, metodo),
              type = "warning", duration = 6)
            next
          }
          df_out[[v]] <- aplicar_transformacion(x, metodo)
        }
      }
      # Eliminar filas con NA introducidas por la transformación
      df_out <- df_out[complete.cases(df_out), , drop = FALSE]
    }
    
    df_out
  })
  
  output$tabla_datos <- renderDT({
    datatable(datos_modelo(),
              options = list(pageLength = 8, scrollX = TRUE,
                             dom = 'tp', searching = FALSE),
              rownames = FALSE) |>
      formatRound(columns = input$vars_seleccionadas, digits = 2)
  })
  
  output$diag_fechas <- renderPrint({
    df <- datos_modelo()
    fecha_col <- names(df)[1]
    fechas <- df[[fecha_col]]
    
    cat("─── Diagnóstico de la serie ───\n")
    cat(sprintf("Observaciones: %d\n", nrow(df)))
    
    if (inherits(fechas, "Date")) {
      cat(sprintf("Rango de fechas: %s  →  %s\n",
                  format(min(fechas, na.rm = TRUE), "%Y-%m-%d"),
                  format(max(fechas, na.rm = TRUE), "%Y-%m-%d")))
      
      # Detectar frecuencia
      diffs <- as.numeric(diff(fechas))
      diff_mediano <- median(diffs, na.rm = TRUE)
      freq <- if (diff_mediano <= 2) "diaria"
      else if (diff_mediano <= 10) "semanal"
      else if (diff_mediano <= 35) "mensual"
      else if (diff_mediano <= 100) "trimestral"
      else "anual"
      cat(sprintf("Frecuencia detectada: %s (Δ mediano = %.0f días)\n",
                  freq, diff_mediano))
      
      # Alertar sobre fechas raras
      if (any(format(fechas, "%Y") < "1950" |
              format(fechas, "%Y") > "2030", na.rm = TRUE)) {
        cat("⚠ ATENCIÓN: hay fechas fuera del rango razonable (1950-2030).\n")
        cat("   Probablemente el formato del CSV no se reconoció correctamente.\n")
      }
    } else {
      cat("⚠ La primera columna no es una fecha válida.\n")
    }
    
    if (isTRUE(input$aplicar_transforms)) {
      cat("\n✓ Transformaciones aplicadas. Las series mostradas son las que",
          "entran al VAR.\n")
    }
  })
  
  output$plot_datos <- renderPlot({
    df <- datos_modelo()
    fecha_col <- names(df)[1]
    df_long <- df |>
      pivot_longer(cols = -all_of(fecha_col),
                   names_to = "variable", values_to = "valor")
    
    titulo <- if (isTRUE(input$aplicar_transforms)) {
      "Series transformadas (las que se usan en el VAR)"
    } else {
      "Series en nivel (sin transformar)"
    }
    
    ggplot(df_long, aes_string(x = fecha_col, y = "valor")) +
      geom_line(color = COLOR_BLUE, linewidth = 0.7) +
      geom_hline(yintercept = 0, linetype = "dotted",
                 color = "grey50", alpha = 0.5) +
      facet_wrap(~ variable, scales = "free_y", ncol = 3) +
      labs(title = titulo, x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(strip.text = element_text(face = "bold", color = COLOR_BLUE),
            plot.title = element_text(face = "bold", color = COLOR_BLUE,
                                      size = 12))
  })
  
  # ─── 2. ESTIMACIÓN DEL VAR ─────────────────────────────────────────────────
  Y_matrix <- reactive({
    df <- datos_modelo()
    as.matrix(df[, input$vars_seleccionadas, drop = FALSE])
  })
  
  fit_var <- eventReactive(input$btn_estimar, {
    Y <- Y_matrix()
    p <- input$p_lags
    tryCatch({
      VAR(Y, p = p, type = input$var_tipo)
    }, error = function(e) {
      showNotification(paste("Error al estimar VAR:", e$message),
                       type = "error", duration = 8)
      NULL
    })
  })
  
  output$varselect_out <- renderPrint({
    Y <- Y_matrix()
    if (nrow(Y) > 8) {
      sel <- VARselect(Y, lag.max = 8, type = input$var_tipo)
      cat("Criterios de selección de rezagos (p óptimo según cada criterio):\n\n")
      print(sel$selection)
      cat("\nValores por rezago:\n")
      print(round(sel$criteria, 4))
    } else {
      cat("Necesitas más observaciones para correr VARselect.")
    }
  })
  
  output$var_roots <- renderPrint({
    fit <- fit_var()
    req(fit)
    rt <- roots(fit)
    cat("Raíces del polinomio característico:\n")
    print(round(rt, 4))
    cat("\n")
    if (max(rt) < 1) {
      cat("✓ El VAR es ESTABLE (todas las raíces < 1).\n")
    } else {
      cat("✗ ATENCIÓN: VAR INESTABLE. Considera reducir p o transformar variables.\n")
    }
  })
  
  base_var <- reactive({
    fit <- fit_var()
    req(fit)
    fc <- predict(fit, n.ahead = input$horizonte, ci = 0.95)
    sapply(fc$fcst, function(x) x[, "fcst"])
  })
  
  output$base_var_summary <- renderPrint({
    bv <- base_var()
    cat("Pronóstico baseline (primeros 4 horizontes):\n\n")
    print(round(head(bv, 4), 3))
  })
  
  # ─── 3. ENFOQUE DETERMINÍSTICO ─────────────────────────────────────────────
  output$shock_sliders <- renderUI({
    vars <- input$vars_seleccionadas
    req(vars)
    lapply(vars, function(v) {
      sliderInput(paste0("shock_", v), v,
                  min = -5, max = 5, value = 0, step = 0.5)
    })
  })
  
  escenario_determ <- eventReactive(input$btn_determ, {
    fit <- fit_var(); req(fit)
    bv <- base_var(); req(bv)
    H  <- input$horizonte
    K  <- ncol(bv)
    Sigma <- summary(fit)$covres
    sd_struct <- sqrt(diag(Sigma))
    
    if (input$modo_determ == "puntual") {
      escenario <- bv
      attr(escenario, "tipo") <- "Pronóstico puntual"
    } else {
      vars <- input$vars_seleccionadas
      shock_vec <- sapply(vars, function(v) input[[paste0("shock_", v)]])
      shock_vec[is.null(shock_vec)] <- 0
      pers <- input$persistencia
      patron <- c(rep(1, pers),
                  seq(1, 0, length.out = H - pers + 1)[-1])
      patron <- patron[1:H]
      shocks <- t(sapply(seq_len(H), function(h) {
        patron[h] * shock_vec * sd_struct
      }))
      escenario <- bv + shocks
      attr(escenario, "tipo") <- "Pronóstico + shocks calibrados"
    }
    colnames(escenario) <- input$vars_seleccionadas
    escenario
  })
  
  output$plot_determ <- renderPlot({
    esc <- escenario_determ()
    req(esc)
    Y <- Y_matrix()
    df <- datos_modelo()
    fecha_col <- names(df)[1]
    
    n_hist <- min(24, nrow(Y))
    H <- nrow(esc)
    last_date <- df[[fecha_col]][nrow(df)]
    fechas_proy <- seq(last_date, by = "quarter", length.out = H + 1)[-1]
    
    hist_df <- as.data.frame(tail(Y, n_hist))
    hist_df$fecha <- tail(df[[fecha_col]], n_hist)
    hist_df$tipo  <- "Histórico"
    
    proy_df <- as.data.frame(esc)
    proy_df$fecha <- fechas_proy
    proy_df$tipo  <- attr(esc, "tipo")
    
    plot_df <- bind_rows(hist_df, proy_df) |>
      pivot_longer(cols = -c(fecha, tipo),
                   names_to = "variable", values_to = "valor")
    
    ggplot(plot_df, aes(x = fecha, y = valor, color = tipo)) +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = last_date, linetype = "dashed",
                 color = "grey50", alpha = 0.6) +
      facet_wrap(~ variable, scales = "free_y", ncol = 3) +
      scale_color_manual(values = c("Histórico" = COLOR_GREY,
                                    "Pronóstico puntual" = COLOR_BLUE,
                                    "Pronóstico + shocks calibrados" = COLOR_RED)) +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom",
            strip.text = element_text(face = "bold", color = COLOR_BLUE),
            axis.title = element_blank()) +
      labs(color = NULL)
  })
  
  output$tabla_determ <- renderDT({
    esc <- escenario_determ()
    req(esc)
    df <- as.data.frame(esc)
    df$horizonte <- seq_len(nrow(df))
    df <- df[, c("horizonte", input$vars_seleccionadas)]
    datatable(df, options = list(pageLength = 12, dom = 'tp'),
              rownames = FALSE) |>
      formatRound(columns = input$vars_seleccionadas, digits = 3)
  })
  
  output$dl_determ <- downloadHandler(
    filename = function() paste0("escenario_deterministico_",
                                 Sys.Date(), ".csv"),
    content = function(file) {
      esc <- escenario_determ()
      df <- as.data.frame(esc)
      df$horizonte <- seq_len(nrow(df))
      write.csv(df[, c("horizonte", input$vars_seleccionadas)],
                file, row.names = FALSE)
    }
  )
  
  # ─── 4. MONTE CARLO: tres enfoques ─────────────────────────────────────────
  resultados_mc <- eventReactive(input$btn_mc, {
    fit <- fit_var(); req(fit)
    bv <- base_var(); req(bv)
    set.seed(input$semilla)
    
    H <- input$horizonte
    K <- ncol(bv)
    M <- input$M_sim
    Sigma <- summary(fit)$covres
    sd_struct <- sqrt(diag(Sigma))
    vars <- input$vars_seleccionadas
    
    # --- Simulación base ---
    sim_paths <- array(NA, dim = c(H, K, M),
                       dimnames = list(NULL, vars, NULL))
    
    if (input$motor_mc == "normal") {
      for (m in 1:M) {
        shocks_m <- mvrnorm(H, mu = rep(0, K), Sigma = Sigma)
        for (h in 1:H) sim_paths[h, , m] <- bv[h, ] + shocks_m[h, ]
      }
    } else {
      res_var <- residuals(fit)
      T_eff <- nrow(res_var)
      for (m in 1:M) {
        idx <- sample(seq_len(T_eff), size = H, replace = TRUE)
        shocks_m <- res_var[idx, , drop = FALSE]
        for (h in 1:H) sim_paths[h, , m] <- bv[h, ] + shocks_m[h, ]
      }
    }
    
    # --- ENFOQUE A: percentiles marginales ---
    a_adv <- input$alpha_adv / 100
    a_sev <- input$alpha_sev / 100
    
    A_p50 <- apply(sim_paths, c(1,2), quantile, probs = 0.50, na.rm = TRUE)
    A_adv <- apply(sim_paths, c(1,2), quantile, probs = a_adv, na.rm = TRUE)
    A_sev <- apply(sim_paths, c(1,2), quantile, probs = a_sev, na.rm = TRUE)
    
    # --- ENFOQUE B: trayectoria conjunta ---
    sd_marg <- apply(sim_paths, 2, sd, na.rm = TRUE)
    severity <- apply(sim_paths, 3, function(p) {
      sum(scale(p, center = FALSE, scale = sd_marg))
    })
    pick <- function(s, prob) which.min(abs(s - quantile(s, prob)))
    
    B_p50 <- sim_paths[, , pick(severity, 0.50)]
    B_adv <- sim_paths[, , pick(severity, 1 - a_adv)]
    B_sev <- sim_paths[, , pick(severity, 1 - a_sev)]
    
    # --- ENFOQUE C: shocks calibrados ---
    pers  <- input$crisis_persist
    sigma <- input$crisis_sigma
    patron <- c(rep(1, pers),
                seq(1, 0, length.out = H - pers + 1)[-1])
    patron <- patron[1:H]
    direccion <- ifelse(colMeans(residuals(fit)) >= 0, -1, 1)
    direccion <- sign(diag(Sigma))   # heurística por defecto: negativa
    direccion <- rep(-1, K); names(direccion) <- vars
    if ("crecimiento" %in% vars) direccion["crecimiento"] <- -1
    for (v in c("inflacion","interbancaria","deprec_fx","embi"))
      if (v %in% vars) direccion[v] <- 1
    
    shocks_C_adv <- t(sapply(seq_len(H), function(h)
      sigma * patron[h] * direccion * sd_struct))
    shocks_C_sev <- t(sapply(seq_len(H), function(h)
      (sigma * 1.5) * patron[h] * direccion * sd_struct))
    
    ruido_adv <- mvrnorm(H, mu = rep(0, K), Sigma = 0.25 * Sigma)
    ruido_sev <- mvrnorm(H, mu = rep(0, K), Sigma = 0.25 * Sigma)
    
    C_p50 <- B_p50
    C_adv <- bv + shocks_C_adv + ruido_adv
    C_sev <- bv + shocks_C_sev + ruido_sev
    colnames(C_adv) <- colnames(C_sev) <- vars
    
    # --- Diagnóstico de coherencia (Enfoque A) ---
    var_diag <- vars[1]
    hits <- sum(sapply(1:M, function(m) {
      all(sim_paths[, var_diag, m] <= A_adv[, var_diag])
    }))
    
    list(
      sim_paths = sim_paths,
      A = list(p50 = A_p50, adv = A_adv, sev = A_sev),
      B = list(p50 = B_p50, adv = B_adv, sev = B_sev),
      C = list(p50 = C_p50, adv = C_adv, sev = C_sev),
      severity = severity,
      hits_A = hits,
      M = M, H = H, vars = vars,
      a_adv = a_adv, a_sev = a_sev
    )
  })
  
  output$plot_mc <- renderPlot({
    res <- resultados_mc(); req(res)
    enfoque <- input$vista_mc
    df <- datos_modelo()
    fecha_col <- names(df)[1]
    last_date <- df[[fecha_col]][nrow(df)]
    H <- res$H
    fechas_proy <- seq(last_date, by = "quarter", length.out = H + 1)[-1]
    n_hist <- min(20, nrow(df))
    
    # Banda de la nube MC para contextualizar
    q_lo <- apply(res$sim_paths, c(1,2), quantile, 0.025, na.rm = TRUE)
    q_hi <- apply(res$sim_paths, c(1,2), quantile, 0.975, na.rm = TRUE)
    
    esc <- res[[enfoque]]
    
    plots <- lapply(res$vars, function(v) {
      hist_v <- data.frame(
        fecha = tail(df[[fecha_col]], n_hist),
        valor = tail(df[[v]], n_hist),
        tipo  = "Histórico"
      )
      proy <- data.frame(
        fecha = rep(fechas_proy, 3),
        valor = c(esc$p50[, v], esc$adv[, v], esc$sev[, v]),
        tipo  = rep(c("Baseline (p50)",
                      sprintf("Adverso (p%d)", round(res$a_adv*100)),
                      sprintf("Severo (p%g)", res$a_sev*100)), each = H)
      )
      banda <- data.frame(
        fecha = fechas_proy,
        lo = q_lo[, v], hi = q_hi[, v]
      )
      
      ggplot() +
        geom_ribbon(data = banda, aes(x = fecha, ymin = lo, ymax = hi),
                    fill = COLOR_GREY, alpha = 0.18) +
        geom_line(data = hist_v, aes(x = fecha, y = valor),
                  color = COLOR_GREY, linewidth = 0.7) +
        geom_line(data = proy, aes(x = fecha, y = valor, color = tipo),
                  linewidth = 0.9) +
        geom_vline(xintercept = last_date, linetype = "dashed",
                   color = "grey50", alpha = 0.6) +
        scale_color_manual(values = setNames(
          c(COLOR_BLUE, COLOR_GOLD, COLOR_RED),
          c("Baseline (p50)",
            sprintf("Adverso (p%d)", round(res$a_adv*100)),
            sprintf("Severo (p%g)", res$a_sev*100)))) +
        labs(title = v, x = NULL, y = NULL, color = NULL) +
        theme_minimal(base_size = 10) +
        theme(plot.title = element_text(face = "bold", color = COLOR_BLUE,
                                        size = 11),
              legend.position = "none")
    })
    
    # Patchwork con leyenda compartida abajo
    combined <- wrap_plots(plots, ncol = 3) +
      plot_annotation(
        title = paste("Enfoque", enfoque, "—",
                      switch(enfoque,
                             A = "Percentiles marginales",
                             B = "Trayectoria conjunta",
                             C = "Shocks calibrados a crisis")),
        theme = theme(plot.title = element_text(face = "bold",
                                                color = COLOR_BLUE, size = 14))
      )
    
    # Añadir una leyenda manual con el primer plot
    legend_plot <- plots[[1]] + theme(legend.position = "bottom")
    combined / guide_area() + plot_layout(heights = c(10, 1), guides = "collect") &
      theme(legend.position = "bottom")
  })
  
  output$mc_diagnostico <- renderPrint({
    res <- resultados_mc(); req(res)
    cat("=== DIAGNÓSTICO MONTE CARLO ===\n\n")
    cat(sprintf("Simulaciones: M = %d, Horizonte: H = %d, Variables: K = %d\n",
                res$M, res$H, length(res$vars)))
    cat(sprintf("Motor: %s\n",
                if (input$motor_mc == "normal") "Normal con Cholesky"
                else "Bootstrap residual"))
    cat(sprintf("Percentiles: adverso = %g%%, severo = %g%%\n\n",
                res$a_adv * 100, res$a_sev * 100))
    
    cat("--- Coherencia del Enfoque A (percentiles marginales) ---\n")
    var1 <- res$vars[1]
    cat(sprintf("Trayectorias que cumplen %s[h] <= cuantil_marginal[h] para todo h:\n",
                var1))
    cat(sprintf("  %d de %d (%.2f%%)\n\n",
                res$hits_A, res$M, 100 * res$hits_A / res$M))
    if (res$hits_A / res$M < 0.01) {
      cat("✗ El escenario A NO corresponde a ninguna trayectoria realmente simulada.\n")
      cat("  Es un Frankenstein estadístico: usar A solo como referencia exploratoria.\n\n")
    }
    
    cat("--- Promedios por enfoque (a través del horizonte) ---\n")
    resumen <- data.frame(
      Variable = res$vars,
      A_baseline = colMeans(res$A$p50),
      A_adverso  = colMeans(res$A$adv),
      A_severo   = colMeans(res$A$sev),
      B_adverso  = colMeans(res$B$adv),
      B_severo   = colMeans(res$B$sev),
      C_adverso  = colMeans(res$C$adv),
      C_severo   = colMeans(res$C$sev)
    )
    print(round(resumen, 3), row.names = FALSE)
  })
  
  # ─── 5. COMPARACIÓN ────────────────────────────────────────────────────────
  output$selector_var_comp <- renderUI({
    res <- resultados_mc()
    if (is.null(res)) {
      div(class = "nota",
          "Ejecuta primero el Monte Carlo en el paso 4.")
    } else {
      selectInput("var_comp", "Variable a comparar:",
                  choices = res$vars, selected = res$vars[1])
    }
  })
  
  output$plot_comparacion <- renderPlot({
    res <- resultados_mc(); req(res, input$var_comp)
    v <- input$var_comp
    df <- datos_modelo(); fecha_col <- names(df)[1]
    last_date <- df[[fecha_col]][nrow(df)]
    H <- res$H
    fechas_proy <- seq(last_date, by = "quarter", length.out = H + 1)[-1]
    n_hist <- min(20, nrow(df))
    
    hist_df <- data.frame(
      fecha = tail(df[[fecha_col]], n_hist),
      valor = tail(df[[v]], n_hist),
      enfoque = NA_character_,
      tipo = "Histórico"
    )
    
    build_df <- function(esc, label) {
      data.frame(
        fecha = rep(fechas_proy, 3),
        valor = c(esc$p50[, v], esc$adv[, v], esc$sev[, v]),
        enfoque = label,
        tipo = rep(c("Baseline (p50)",
                     sprintf("Adverso (p%d)", round(res$a_adv*100)),
                     sprintf("Severo (p%g)", res$a_sev*100)), each = H)
      )
    }
    
    proy_df <- bind_rows(
      build_df(res$A, "A) Marginal"),
      build_df(res$B, "B) Conjunto"),
      build_df(res$C, "C) Crisis")
    )
    
    hist_repl <- bind_rows(
      hist_df |> mutate(enfoque = "A) Marginal"),
      hist_df |> mutate(enfoque = "B) Conjunto"),
      hist_df |> mutate(enfoque = "C) Crisis")
    )
    
    plot_df <- bind_rows(hist_repl, proy_df)
    plot_df$tipo <- factor(plot_df$tipo, levels = c(
      "Histórico", "Baseline (p50)",
      sprintf("Adverso (p%d)", round(res$a_adv*100)),
      sprintf("Severo (p%g)", res$a_sev*100)))
    
    ggplot(plot_df, aes(x = fecha, y = valor, color = tipo, linetype = tipo)) +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = last_date, linetype = "dashed",
                 color = "grey50", alpha = 0.6) +
      facet_wrap(~ enfoque, ncol = 3) +
      scale_color_manual(values = setNames(
        c(COLOR_GREY, COLOR_BLUE, COLOR_GOLD, COLOR_RED),
        levels(plot_df$tipo))) +
      scale_linetype_manual(values = setNames(
        c("solid","solid","dashed","solid"),
        levels(plot_df$tipo))) +
      labs(title = paste("Comparación de enfoques —", v),
           x = NULL, y = v, color = NULL, linetype = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom",
            strip.text = element_text(face = "bold", color = COLOR_BLUE,
                                      size = 12),
            plot.title = element_text(face = "bold", color = COLOR_BLUE))
  })
  
  output$dl_comparacion <- downloadHandler(
    filename = function() paste0("comparacion_enfoques_", Sys.Date(), ".csv"),
    content = function(file) {
      res <- resultados_mc(); req(res)
      build <- function(esc, enfoque) {
        df <- as.data.frame(esc$p50); df$tipo <- "baseline"; df$enfoque <- enfoque
        df$horizonte <- seq_len(nrow(df))
        adv <- as.data.frame(esc$adv); adv$tipo <- "adverso"; adv$enfoque <- enfoque
        adv$horizonte <- seq_len(nrow(adv))
        sev <- as.data.frame(esc$sev); sev$tipo <- "severo"; sev$enfoque <- enfoque
        sev$horizonte <- seq_len(nrow(sev))
        bind_rows(df, adv, sev)
      }
      out <- bind_rows(
        build(res$A, "A_marginal"),
        build(res$B, "B_conjunto"),
        build(res$C, "C_crisis")
      )
      out <- out[, c("enfoque","tipo","horizonte", res$vars)]
      write.csv(out, file, row.names = FALSE)
    }
  )
}

# =============================================================================
shinyApp(ui = ui, server = server)