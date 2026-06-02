# ============================================================
# APP SHINY: STRESS TESTING VAR - DETERMINISTICO Y MONTE CARLO
# ============================================================

library(shiny)
library(vars)
library(tidyverse)
library(MASS)
library(ggplot2)

# ------------------------------------------------------------
# FUNCIONES
# ------------------------------------------------------------

simulate_var_path <- function(fit_var, H, Sigma) {
  
  K <- fit_var$K
  p <- fit_var$p
  var_names <- colnames(fit_var$y)
  B <- Bcoef(fit_var)
  
  yhist <- tail(as.matrix(fit_var$y), p)
  colnames(yhist) <- var_names
  
  ysim <- rbind(yhist, matrix(NA, nrow = H, ncol = K))
  colnames(ysim) <- var_names
  
  for (h in 1:H) {
    
    x <- rep(0, ncol(B))
    names(x) <- colnames(B)
    
    for (lag in 1:p) {
      for (v in var_names) {
        cname <- paste0(v, ".l", lag)
        if (cname %in% names(x)) {
          x[cname] <- ysim[p + h - lag, v]
        }
      }
    }
    
    if ("const" %in% names(x)) x["const"] <- 1
    
    shock <- MASS::mvrnorm(1, mu = rep(0, K), Sigma = Sigma)
    y_pred <- as.numeric(B %*% x)
    
    ysim[p + h, ] <- y_pred + shock
  }
  
  out <- ysim[(p + 1):(p + H), ]
  colnames(out) <- var_names
  out
}

make_long <- function(mat, variables, T_total, H, nombre) {
  df <- as.data.frame(mat)
  colnames(df) <- variables
  df$periodo <- (T_total + 1):(T_total + H)
  df$escenario <- nombre
  
  df |>
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable",
      values_to = "valor"
    )
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Stress Testing Macro-Financiero con VAR"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file", "Cargar base CSV", accept = ".csv"),
      
      uiOutput("var_selector"),
      uiOutput("transform_selector"),
      
      numericInput("lags", "Rezagos VAR", value = 2, min = 1, max = 8),
      numericInput("horizon", "Horizonte", value = 8, min = 1, max = 24),
      numericInput("M", "Simulaciones Monte Carlo", value = 5000, min = 500, step = 500),
      numericInput("n_hist", "Histórico a mostrar", value = 24, min = 4, max = 120),
      
      hr(),
      
      h4("Opciones de gráficos"),
      
      checkboxInput(
        "mostrar_historico",
        "Mostrar histórico",
        value = TRUE
      ),
      
      uiOutput("plot_variable_selector"),
      
      checkboxGroupInput(
        "escenarios_grafico",
        "Escenarios a mostrar",
        choices = c(
          "Baseline VAR",
          "A - Marginal p5",
          "A - Marginal p1",
          "B - Conjunta p99",
          "C - Crisis moderada",
          "C - Crisis severa",
          "D - Escenario explícito"
        ),
        selected = c(
          "Baseline VAR",
          "B - Conjunta p99",
          "C - Crisis moderada",
          "C - Crisis severa"
        )
      ),
      
      checkboxInput(
        "mostrar_linea_inicio",
        "Mostrar línea de inicio del escenario",
        value = TRUE
      ),
      
      numericInput(
        "ncol_facets",
        "Columnas en facetas",
        value = 2,
        min = 1,
        max = 4
      ),
      
      hr(),
      
      selectInput(
        "growth_var",
        "Variable de crecimiento",
        choices = NULL
      ),
      
      selectInput(
        "inflation_var",
        "Variable de inflación",
        choices = NULL
      ),
      
      selectInput(
        "rate_var",
        "Variable de tasa nominal",
        choices = NULL
      ),
      
      selectInput(
        "fx_var",
        "Variable cambiaria",
        choices = NULL
      ),
      
      selectInput(
        "spread_var",
        "Variable spread / EMBI",
        choices = NULL
      ),
      
      hr(),
      
      numericInput("shock_sigma", "Shock crisis moderada σ", value = 3.0, min = 0.5, max = 10),
      numericInput("shock_sigma_sev", "Shock crisis severa σ", value = 4.5, min = 0.5, max = 10),
      
      actionButton("run", "Ejecutar stress test", class = "btn-primary"),
      
      hr(),
      
      checkboxInput(
        "usar_escenario_explicito",
        "Usar escenario explícito definido por el usuario",
        value = FALSE
      ),
      
      fileInput(
        "file_escenario",
        "Cargar escenario explícito CSV",
        accept = ".csv"
      ),
      
      helpText(
        "El CSV debe tener columnas: horizonte y las variables seleccionadas del VAR."
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen VAR", verbatimTextOutput("summary_model")),
        tabPanel("Escenarios", plotOutput("plot_scenarios", height = "700px")),
        tabPanel("Tasa real", plotOutput("plot_real_rate", height = "500px")),
        tabPanel("Pérdida real", plotOutput("plot_loss", height = "500px")),
        tabPanel("Resultados", tableOutput("results_table")),
        tabPanel("Percentiles pérdida", tableOutput("loss_percentiles"))
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$plot_variable_selector <- renderUI({
    req(input$vars)
    
    checkboxGroupInput(
      "variables_grafico",
      "Variables a mostrar en gráficos",
      choices = input$vars,
      selected = input$vars
    )
  })
  
  output$var_selector <- renderUI({
    req(datos())
    checkboxGroupInput(
      "vars",
      "Variables del VAR",
      choices = names(datos()),
      selected = names(datos())[1:min(5, ncol(datos()))]
    )
  })
  
  output$transform_selector <- renderUI({
    req(datos())
    
    checkboxGroupInput(
      "growth_transform_vars",
      "Transformar a tasa de crecimiento",
      choices = names(datos()),
      selected = NULL
    )
  })
  
  observeEvent(input$vars, {
    req(input$vars)
    
    cols <- input$vars
    
    updateSelectInput(session, "growth_var", choices = cols, selected = cols[1])
    updateSelectInput(session, "inflation_var", choices = cols, selected = cols[min(2, length(cols))])
    updateSelectInput(session, "rate_var", choices = cols, selected = cols[min(3, length(cols))])
    updateSelectInput(session, "fx_var", choices = cols, selected = cols[min(4, length(cols))])
    updateSelectInput(session, "spread_var", choices = cols, selected = cols[min(5, length(cols))])
  })
  
  resultados <- eventReactive(input$run, {
    
    req(input$vars)
    
    df_modelo <- datos() |>
      dplyr::select(all_of(input$vars))
    
    # Transformar variables seleccionadas a tasa de crecimiento
    # Fórmula: 100 * (log(x_t) - log(x_{t-1}))
    if (!is.null(input$growth_transform_vars)) {
      
      vars_transformar <- intersect(
        input$growth_transform_vars,
        input$vars
      )
      
      for (v in vars_transformar) {
        
        if (any(df_modelo[[v]] <= 0, na.rm = TRUE)) {
          warning(
            paste(
              "La variable", v,
              "tiene valores <= 0. Se usará diferencia porcentual simple."
            )
          )
          
          df_modelo[[v]] <- 100 * (
            df_modelo[[v]] / dplyr::lag(df_modelo[[v]]) - 1
          )
          
        } else {
          
          df_modelo[[v]] <- 100 * (
            log(df_modelo[[v]]) - log(dplyr::lag(df_modelo[[v]]))
          )
        }
      }
    }
    
    Y <- df_modelo |>
      drop_na() |>
      as.matrix()
    
    variables <- colnames(Y)
    H <- input$horizon
    M <- input$M
    K <- ncol(Y)
    T_total <- nrow(Y)
    
    fit_var <- VAR(Y, p = input$lags, type = "const")
    
    fc_var <- predict(fit_var, n.ahead = H, ci = 0.95)
    base_var <- sapply(fc_var$fcst, function(x) x[, "fcst"])
    
    Sigma <- summary(fit_var)$covres
    
    set.seed(123)
    
    sim_paths <- array(
      NA,
      dim = c(H, K, M),
      dimnames = list(
        horizonte = 1:H,
        variable = variables,
        simulacion = 1:M
      )
    )
    
    for (m in 1:M) {
      sim_paths[, , m] <- simulate_var_path(fit_var, H, Sigma)
    }
    
    # --------------------------------------------------------
    # A: Percentiles marginales
    # --------------------------------------------------------
    
    esc_A_p05 <- apply(sim_paths, c(1, 2), quantile, probs = 0.05)
    esc_A_p01 <- apply(sim_paths, c(1, 2), quantile, probs = 0.01)
    
    colnames(esc_A_p05) <- variables
    colnames(esc_A_p01) <- variables
    
    # --------------------------------------------------------
    # B: Trayectoria conjunta extrema
    # --------------------------------------------------------
    
    sd_marg <- apply(Y, 2, sd, na.rm = TRUE)
    
    g <- input$growth_var
    pi <- input$inflation_var
    i <- input$rate_var
    e <- input$fx_var
    s <- input$spread_var
    
    severity <- apply(sim_paths, 3, function(p) {
      
      sev <- 0
      
      if (g %in% variables) sev <- sev - sum(p[, g] / sd_marg[g])
      if (pi %in% variables) sev <- sev + sum(p[, pi] / sd_marg[pi])
      if (i %in% variables) sev <- sev + sum(p[, i] / sd_marg[i])
      if (e %in% variables) sev <- sev + sum(p[, e] / sd_marg[e])
      if (s %in% variables) sev <- sev + sum(p[, s] / sd_marg[s])
      
      sev
    })
    
    m_p99 <- which.min(abs(severity - quantile(severity, 0.99)))
    esc_B_p99 <- sim_paths[, , m_p99]
    
    # --------------------------------------------------------
    # C: Crisis calibrada
    # --------------------------------------------------------
    
    patron <- c(1.0, 1.0, 1.0, 1.0, 0.7, 0.5, 0.3, 0.2)
    patron <- rep(patron, length.out = H)
    
    direccion <- rep(0, K)
    names(direccion) <- variables
    
    if (g %in% variables) direccion[g] <- -1
    if (pi %in% variables) direccion[pi] <- 1
    if (i %in% variables) direccion[i] <- 1
    if (e %in% variables) direccion[e] <- 1
    if (s %in% variables) direccion[s] <- 1
    
    sd_struct <- sqrt(diag(Sigma))
    names(sd_struct) <- variables
    
    shocks_C <- t(sapply(seq_len(H), function(h) {
      input$shock_sigma * patron[h] * direccion * sd_struct
    }))
    
    shocks_C_sev <- t(sapply(seq_len(H), function(h) {
      input$shock_sigma_sev * patron[h] * direccion * sd_struct
    }))
    
    set.seed(456)
    
    esc_C <- base_var + shocks_C +
      MASS::mvrnorm(H, mu = rep(0, K), Sigma = 0.25 * Sigma)
    
    esc_C_sev <- base_var + shocks_C_sev +
      MASS::mvrnorm(H, mu = rep(0, K), Sigma = 0.25 * Sigma)
    
    colnames(esc_C) <- variables
    colnames(esc_C_sev) <- variables
    
    # --------------------------------------------------------
    # D: Escenario explícito definido por el usuario
    # --------------------------------------------------------
    
    esc_D <- NULL
    
    if (isTRUE(input$usar_escenario_explicito) && !is.null(input$file_escenario)) {
      
      escenario_usuario <- read.csv(input$file_escenario$datapath)
      
      # Validaciones mínimas
      if (!"horizonte" %in% names(escenario_usuario)) {
        stop("El CSV del escenario explícito debe contener una columna llamada 'horizonte'.")
      }
      
      faltantes <- setdiff(variables, names(escenario_usuario))
      
      if (length(faltantes) > 0) {
        stop(
          paste(
            "El escenario explícito no contiene estas variables:",
            paste(faltantes, collapse = ", ")
          )
        )
      }
      
      escenario_usuario <- escenario_usuario |>
        arrange(horizonte) |>
        dplyr::select(all_of(variables))
      
      if (nrow(escenario_usuario) < H) {
        stop("El escenario explícito tiene menos filas que el horizonte seleccionado.")
      }
      
      esc_D <- escenario_usuario[1:H, ] |>
        as.matrix()
      
      colnames(esc_D) <- variables
    }
    
    # --------------------------------------------------------
    # Tasa real y pérdida
    # --------------------------------------------------------
    
    tasa_real_mc <- sim_paths[, i, ] - sim_paths[, pi, ]
    
    evento_mc_4t <- apply(
      tasa_real_mc[1:4, ],
      2,
      function(x) all(x < 0)
    )
    
    prob_mc_4t <- mean(evento_mc_4t)
    
    perdida_mc_4t <- apply(
      tasa_real_mc[1:4, ],
      2,
      function(x) sum(pmin(x, 0))
    )
    
    percentiles_perdida <- quantile(
      perdida_mc_4t,
      probs = c(0.01, 0.05, 0.10, 0.50, 0.90, 0.95, 0.99)
    )
    
    calc_ind <- function(mat, nombre) {
      tr <- mat[, i] - mat[, pi]
      tibble(
        escenario = nombre,
        tasa_real_min_4t = min(tr[1:4]),
        perdida_real_acum_4t = sum(pmin(tr[1:4], 0)),
        tasa_real_negativa_4t = all(tr[1:4] < 0)
      )
    }
    
    tabla_resultados <- bind_rows(
      calc_ind(base_var, "Baseline VAR"),
      calc_ind(esc_A_p05, "A - Marginal p5"),
      calc_ind(esc_A_p01, "A - Marginal p1"),
      calc_ind(esc_B_p99, "B - Conjunta p99"),
      calc_ind(esc_C, "C - Crisis moderada"),
      calc_ind(esc_C_sev, "C - Crisis severa"),
      if (!is.null(esc_D)) calc_ind(esc_D, "D - Escenario explícito")
    ) |>
      mutate(
        probabilidad_MC_4t = prob_mc_4t,
        alerta = case_when(
          prob_mc_4t >= 0.30 ~ "ALERTA ROJA",
          prob_mc_4t >= 0.15 ~ "ALERTA AMARILLA",
          TRUE ~ "SIN ALERTA"
        )
      )
    
    list(
      Y = Y,
      variables = variables,
      fit_var = fit_var,
      base_var = base_var,
      sim_paths = sim_paths,
      esc_A_p05 = esc_A_p05,
      esc_A_p01 = esc_A_p01,
      esc_B_p99 = esc_B_p99,
      esc_C = esc_C,
      esc_C_sev = esc_C_sev,
      esc_D = esc_D,
      perdida_mc_4t = perdida_mc_4t,
      percentiles_perdida = percentiles_perdida,
      tabla_resultados = tabla_resultados,
      T_total = T_total,
      H = H,
      n_hist = input$n_hist,
      pi = pi,
      i = i
    )
  })
  
  output$summary_model <- renderPrint({
    req(resultados())
    summary(resultados()$fit_var)
  })
  
  output$results_table <- renderTable({
    req(resultados())
    resultados()$tabla_resultados
  })
  
  output$loss_percentiles <- renderTable({
    req(resultados())
    tibble(
      percentil = names(resultados()$percentiles_perdida),
      perdida_real_acum_4t = as.numeric(resultados()$percentiles_perdida)
    )
  })
  
  output$plot_scenarios <- renderPlot({
    
    r <- resultados()
    
    Y <- r$Y
    variables <- r$variables
    T_total <- r$T_total
    H <- r$H
    n_hist <- r$n_hist
    
    vars_plot <- intersect(input$variables_grafico, variables)
    escenarios_plot <- input$escenarios_grafico
    
    req(length(vars_plot) > 0)
    
    historico <- as.data.frame(Y) |>
      tail(n_hist)
    
    historico$periodo <- (T_total - nrow(historico) + 1):T_total
    historico$escenario <- "Histórico"
    
    historico_long <- historico |>
      pivot_longer(
        cols = all_of(variables),
        names_to = "variable",
        values_to = "valor"
      ) |>
      filter(variable %in% vars_plot)
    
    bloques <- list()
    
    if (isTRUE(input$mostrar_historico)) {
      bloques <- append(bloques, list(historico_long))
    }
    
    if ("Baseline VAR" %in% escenarios_plot) {
      bloques <- append(
        bloques,
        list(make_long(r$base_var, variables, T_total, H, "Baseline VAR"))
      )
    }
    
    if ("A - Marginal p5" %in% escenarios_plot) {
      bloques <- append(
        bloques,
        list(make_long(r$esc_A_p05, variables, T_total, H, "A - Marginal p5"))
      )
    }
    
    if ("A - Marginal p1" %in% escenarios_plot) {
      bloques <- append(
        bloques,
        list(make_long(r$esc_A_p01, variables, T_total, H, "A - Marginal p1"))
      )
    }
    
    if ("B - Conjunta p99" %in% escenarios_plot) {
      bloques <- append(
        bloques,
        list(make_long(r$esc_B_p99, variables, T_total, H, "B - Conjunta p99"))
      )
    }
    
    if ("C - Crisis moderada" %in% escenarios_plot) {
      bloques <- append(
        bloques,
        list(make_long(r$esc_C, variables, T_total, H, "C - Crisis moderada"))
      )
    }
    
    if ("C - Crisis severa" %in% escenarios_plot) {
      bloques <- append(
        bloques,
        list(make_long(r$esc_C_sev, variables, T_total, H, "C - Crisis severa"))
      )
    }
    
    if ("D - Escenario explícito" %in% escenarios_plot && !is.null(r$esc_D)) {
      bloques <- append(
        bloques,
        list(make_long(r$esc_D, variables, T_total, H, "D - Escenario explícito"))
      )
    }
    
    plot_data <- bind_rows(bloques) |>
      filter(variable %in% vars_plot)
    
    grafico <- ggplot(
      plot_data,
      aes(x = periodo, y = valor, color = escenario)
    ) +
      geom_line(linewidth = 1) +
      facet_wrap(
        ~ variable,
        scales = "free_y",
        ncol = input$ncol_facets
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold")
      ) +
      labs(
        title = "Escenarios VAR",
        subtitle = paste("Histórico mostrado:", n_hist, "observaciones"),
        x = "Periodo",
        y = "Valor",
        color = ""
      )
    
    if (isTRUE(input$mostrar_linea_inicio)) {
      grafico <- grafico +
        geom_vline(
          xintercept = T_total,
          linetype = "dashed"
        )
    }
    
    grafico
  })  
  
  output$plot_real_rate <- renderPlot({
    
    r <- resultados()
    
    Y <- r$Y
    T_total <- r$T_total
    H <- r$H
    n_hist <- r$n_hist
    i <- r$i
    pi <- r$pi
    
    escenarios_plot <- input$escenarios_grafico
    
    tasa_real_hist <- as.data.frame(Y) |>
      mutate(
        tasa_real = .data[[i]] - .data[[pi]],
        periodo = seq_len(nrow(Y)),
        escenario = "Histórico"
      ) |>
      tail(n_hist) |>
      select(periodo, tasa_real, escenario)
    
    make_real <- function(mat, nombre) {
      tibble(
        periodo = (T_total + 1):(T_total + H),
        tasa_real = mat[, i] - mat[, pi],
        escenario = nombre
      )
    }
    
    bloques <- list()
    
    if (isTRUE(input$mostrar_historico)) {
      bloques <- append(bloques, list(tasa_real_hist))
    }
    
    if ("Baseline VAR" %in% escenarios_plot) {
      bloques <- append(bloques, list(make_real(r$base_var, "Baseline VAR")))
    }
    
    if ("A - Marginal p5" %in% escenarios_plot) {
      bloques <- append(bloques, list(make_real(r$esc_A_p05, "A - Marginal p5")))
    }
    
    if ("A - Marginal p1" %in% escenarios_plot) {
      bloques <- append(bloques, list(make_real(r$esc_A_p01, "A - Marginal p1")))
    }
    
    if ("B - Conjunta p99" %in% escenarios_plot) {
      bloques <- append(bloques, list(make_real(r$esc_B_p99, "B - Conjunta p99")))
    }
    
    if ("C - Crisis moderada" %in% escenarios_plot) {
      bloques <- append(bloques, list(make_real(r$esc_C, "C - Crisis moderada")))
    }
    
    if ("C - Crisis severa" %in% escenarios_plot) {
      bloques <- append(bloques, list(make_real(r$esc_C_sev, "C - Crisis severa")))
    }
    
    if ("D - Escenario explícito" %in% escenarios_plot && !is.null(r$esc_D)) {
      bloques <- append(bloques, list(make_real(r$esc_D, "D - Escenario explícito")))
    }
    
    tasa_real_plot <- bind_rows(bloques)
    
    grafico <- ggplot(
      tasa_real_plot,
      aes(x = periodo, y = tasa_real, color = escenario)
    ) +
      geom_hline(yintercept = 0, linetype = "dotted") +
      geom_line(linewidth = 1.1) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold")
      ) +
      labs(
        title = "Tasa real por escenario",
        subtitle = "Tasa real aproximada = tasa nominal - inflación",
        x = "Periodo",
        y = "Tasa real",
        color = ""
      )
    
    if (isTRUE(input$mostrar_linea_inicio)) {
      grafico <- grafico +
        geom_vline(xintercept = T_total, linetype = "dashed")
    }
    
    grafico
  })
  
  output$plot_loss <- renderPlot({
    
    r <- resultados()
    
    perdida_df <- tibble(
      perdida_real_acum_4t = r$perdida_mc_4t
    )
    
    ggplot(perdida_df, aes(x = perdida_real_acum_4t)) +
      geom_histogram(bins = 40) +
      geom_vline(
        xintercept = quantile(r$perdida_mc_4t, 0.05),
        linetype = "dashed"
      ) +
      theme_minimal(base_size = 13) +
      labs(
        title = "Distribución Monte Carlo de pérdida real acumulada",
        subtitle = "Primeros cuatro trimestres",
        x = "Pérdida real acumulada",
        y = "Frecuencia"
      )
  })
}

shinyApp(ui, server)