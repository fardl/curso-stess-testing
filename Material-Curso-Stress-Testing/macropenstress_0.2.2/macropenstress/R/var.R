
#' Proyección determinística de un VAR
#'
#' Genera una trayectoria baseline recursiva y permite inyectar un shock en t+1.
#'
#' @param fit Modelo VAR estimado con `vars::VAR`.
#' @param n.ahead Horizonte.
#' @param shocks_t1 Vector opcional de shocks en t+1 con nombres de variables.
#' @return Matriz H x K con la trayectoria proyectada.
proyectar_var_deterministico <- function(fit, n.ahead, shocks_t1 = NULL) {
  K <- fit$K
  p <- fit$p
  B <- vars::Bcoef(fit)
  Y_data <- as.matrix(fit$y)
  var_names <- colnames(Y_data)

  y_path <- rbind(tail(Y_data, p), matrix(NA_real_, n.ahead, K))
  colnames(y_path) <- var_names

  for (hh in seq_len(n.ahead)) {
    lags <- as.vector(t(y_path[(p + hh - 1):hh, , drop = FALSE]))
    z_t <- c(lags, 1)
    if (ncol(B) != length(z_t)) {
      names(z_t) <- colnames(B)
      z_t <- z_t[colnames(B)]
    }
    y_new <- as.numeric(B %*% z_t)
    names(y_new) <- var_names
    if (hh == 1 && !is.null(shocks_t1)) {
      shocks <- rep(0, K); names(shocks) <- var_names
      shocks[names(shocks_t1)] <- shocks_t1
      y_new <- y_new + shocks
    }
    y_path[p + hh, ] <- y_new
  }
  y_path[(p + 1):(p + n.ahead), , drop = FALSE]
}



#' Escenario adverso VAR por shock calibrado
#'
#' Construye un escenario adverso imponiendo un vector de shocks calibrados
#' y dejando que la dinamica del VAR propague los efectos en el horizonte.
#' El shock puede expresarse en unidades de la variable (`tipo = "nivel"`) o
#' como múltiplos de la desviación estándar de los residuos del VAR
#' (`tipo = "sigma"`).
#'
#' @param modelo_var Modelo VAR estimado con `vars::VAR`.
#' @param shock Vector, data frame o matriz con shocks. Si es vector, debe tener
#'   nombres de variables del VAR y se aplica en `shock_periodo`. Si es matriz/data.frame,
#'   las filas representan horizontes y las columnas variables.
#' @param h Horizonte de proyección.
#' @param tipo "nivel" o "sigma". En "sigma", el shock se multiplica por la
#'   desviación estándar residual de cada variable.
#' @param shock_periodo Período en el que se aplica el shock cuando `shock` es vector.
#' @param n_hist_plot Número de observaciones históricas para el gráfico.
#' @param fechas Vector opcional de fechas observadas usadas para estimar el VAR.
#' @param frecuencia Frecuencia temporal: "auto", "mensual", "trimestral", "anual" o "diaria".
#' @param nombre_escenario Nombre del escenario adverso.
#' @return Lista con `resultados`, `datos_grafico`, `graficos`, `baseline`, `adverso` y `shocks`.
escenario_var_shock <- function(modelo_var,
                                shock,
                                h = 12,
                                tipo = c("nivel", "sigma"),
                                shock_periodo = 1,
                                n_hist_plot = 24,
                                fechas = NULL,
                                frecuencia = c("auto", "mensual", "trimestral", "anual", "diaria"),
                                nombre_escenario = "Adverso shock VAR") {
  tipo <- match.arg(tipo)
  frecuencia <- match.arg(frecuencia)

  Y <- as.matrix(modelo_var$y)
  variables <- colnames(Y)
  K <- ncol(Y)
  p <- modelo_var$p
  T_total <- nrow(Y)

  variables <- unname(colnames(Y))

  #--------------------------------------------
  # 1. Fechas
  #--------------------------------------------
  usar_fechas <- !is.null(fechas)
  if (usar_fechas) {
    fechas <- as.Date(fechas)
    if (length(fechas) < T_total) {
      stop("`fechas` debe tener al menos tantas observaciones como las usadas en el VAR.")
    }
    fechas_obs_full <- tail(fechas, T_total)
    fechas_fc <- .inferir_fechas_forecast(fechas_obs_full, h, frecuencia)
  } else {
    fechas_obs_full <- NULL
    fechas_fc <- NULL
  }

  #--------------------------------------------
  # 2. Baseline VAR por predict()
  #--------------------------------------------
  fc <- stats::predict(modelo_var, n.ahead = h, ci = 0.95)
  base_var <- sapply(fc$fcst, function(x) x[, "fcst"])
  colnames(base_var) <- variables

  #--------------------------------------------
  # 3. Matriz de shocks H x K
  #--------------------------------------------
  shocks_mat <- matrix(0, nrow = h, ncol = K)
  colnames(shocks_mat) <- variables

  if (is.vector(shock) && is.numeric(shock) && is.null(dim(shock))) {
    if (is.null(names(shock))) {
      stop("Si `shock` es vector, debe tener nombres de variables del VAR.")
    }
    faltantes <- setdiff(names(shock), variables)
    if (length(faltantes) > 0) {
      stop("Variables del shock no están en el VAR: ", paste(faltantes, collapse = ", "))
    }
    if (shock_periodo < 1 || shock_periodo > h) {
      stop("`shock_periodo` debe estar entre 1 y h.")
    }
    shocks_mat[shock_periodo, names(shock)] <- as.numeric(shock)
  } else {
    shock_df <- as.data.frame(shock)
    if (nrow(shock_df) == 0) stop("`shock` no puede estar vacío.")
    if (is.null(colnames(shock_df))) stop("`shock` debe tener columnas con nombres de variables.")
    faltantes <- setdiff(colnames(shock_df), variables)
    if (length(faltantes) > 0) {
      stop("Variables del shock no están en el VAR: ", paste(faltantes, collapse = ", "))
    }
    n_shock <- min(nrow(shock_df), h)
    shocks_mat[seq_len(n_shock), colnames(shock_df)] <- as.matrix(shock_df[seq_len(n_shock), , drop = FALSE])
  }

  if (tipo == "sigma") {
    Sigma <- summary(modelo_var)$covres
    sd_res <- sqrt(diag(Sigma))
    names(sd_res) <- variables
    shocks_mat <- sweep(shocks_mat, 2, sd_res, `*`)
  }

  #--------------------------------------------
  # 4. Proyección adversa recursiva:
  #    y_{t+1}^{adv} = c + A_1 y_t + ... + A_p y_{t-p+1} + s_{t+1}
  #--------------------------------------------
  B <- vars::Bcoef(modelo_var)
  yhist <- tail(Y, p)
  ysim <- rbind(yhist, matrix(NA_real_, nrow = h, ncol = K))
  colnames(ysim) <- variables

  for (step in seq_len(h)) {
    x <- rep(0, ncol(B))
    names(x) <- colnames(B)

    for (lag in seq_len(p)) {
      for (v in variables) {
        cname <- paste0(v, ".l", lag)
        if (cname %in% names(x)) {
          x[cname] <- ysim[p + step - lag, v]
        }
      }
    }

    if ("const" %in% names(x)) x["const"] <- 1
    if ("trend" %in% names(x)) x["trend"] <- T_total + step
    if ("both" %in% names(x)) x["both"] <- 1

    y_pred <- as.numeric(B %*% x)
    names(y_pred) <- variables

    ysim[p + step, ] <- y_pred + shocks_mat[step, ]
  }

  adverso <- ysim[(p + 1):(p + h), , drop = FALSE]
  colnames(adverso) <- variables

  #--------------------------------------------
  # 5. Data frames de resultados
  #--------------------------------------------
  baseline_long <- .mat_to_long_var(base_var, "Baseline VAR", "Baseline", variables, T_total)
  adverso_long  <- .mat_to_long_var(adverso, nombre_escenario, "Shock calibrado", variables, T_total)

  resultados <- dplyr::bind_rows(baseline_long, adverso_long)

  if (usar_fechas) {
    mapa_fechas_fc <- tibble::tibble(
      periodo = T_total + seq_len(h),
      fecha = fechas_fc
    )
    resultados <- resultados |>
      dplyr::left_join(mapa_fechas_fc, by = "periodo")
  }

  hist_df <- utils::tail(as.data.frame(Y), n_hist_plot)
  hist_df$periodo <- (T_total - nrow(hist_df)) + seq_len(nrow(hist_df))
  if (usar_fechas) hist_df$fecha <- utils::tail(fechas_obs_full, nrow(hist_df))

  hist_long <- hist_df |>
    tidyr::pivot_longer(cols = dplyr::all_of(unname(variables)),
                        names_to = "variable", values_to = "valor") |>
    dplyr::mutate(escenario = "Histórico", enfoque = "Histórico")

  # Ancla para conectar último dato histórico con cada escenario futuro
  ultimo_hist <- hist_long |>
    dplyr::filter(.data$periodo == T_total) |>
    dplyr::select(dplyr::any_of(c("variable", "periodo", "fecha", "valor")))

  escenarios_futuros <- unique(resultados$escenario)
  anclas <- tidyr::crossing(
    ultimo_hist,
    escenario = escenarios_futuros
  ) |>
    dplyr::mutate(enfoque = "Punto de inicio")

  plot_data <- dplyr::bind_rows(hist_long, anclas, resultados) |>
    dplyr::mutate(
      escenario = factor(
        .data$escenario,
        levels = unique(c("Histórico", "Baseline VAR", nombre_escenario))
      )
    )

  x_col <- if (usar_fechas) "fecha" else "periodo"
  x_label <- if (usar_fechas) {
    if (frecuencia == "trimestral") "Fecha (trimestres)" else if (frecuencia == "anual") "Fecha (años)" else if (frecuencia == "mensual") "Fecha (meses)" else "Fecha"
  } else {
    "Periodo"
  }
  x_intercept <- if (usar_fechas) as.numeric(utils::tail(fechas_obs_full, 1)) else T_total

  grafico_panel <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data[[x_col]],
      y = .data$valor,
      color = .data$escenario,
      group = interaction(.data$variable, .data$escenario)
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = x_intercept, linetype = "dashed") +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 11),
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = "Escenario VAR por shock calibrado",
      subtitle = "Shock inicial y propagación dinámica por el VAR",
      x = x_label,
      y = "Valor",
      color = "Escenario"
    )

  if (usar_fechas) {
    grafico_panel <- grafico_panel +
      ggplot2::scale_x_date(
        date_breaks = if (frecuencia == "mensual") "6 months" else if (frecuencia == "trimestral") "1 year" else "2 years",
        date_labels = if (frecuencia == "mensual") "%Y-%m" else "%Y"
      )
  }

  shocks_long <- as.data.frame(shocks_mat) |>
    dplyr::mutate(horizonte = seq_len(h)) |>
    tidyr::pivot_longer(cols = dplyr::all_of(unname(variables)),
                        names_to = "variable", values_to = "shock")

  list(
    resultados = resultados,
    datos_grafico = plot_data,
    graficos = list(panel = grafico_panel),
    baseline = as.data.frame(base_var),
    adverso = as.data.frame(adverso),
    shocks = shocks_long,
    forecast = fc
  )
}

#' Forecast VAR condicionado a sendas exógenamente impuestas
#'
#' Impone valores de una o más variables en cada horizonte y propaga el resto
#' por la dinámica del VAR estimado.
#'
#' @param var_model Modelo VAR estimado con `vars::VAR`.
#' @param paths_conditioned Data frame o matriz H x q con columnas de variables condicionadas.
#' @param ystart Historia inicial opcional con p filas y K columnas.
#' @return Data frame H x K más columna `horizonte`.
forecast_var_condicional <- function(var_model,
                                     paths_conditioned,
                                     h = NULL,
                                     ystart = NULL,
                                     fill_method = c("recursive", "baseline")) {
  fill_method <- match.arg(fill_method)

  K <- var_model$K
  p <- var_model$p
  var_names <- colnames(var_model$y)
  B <- vars::Bcoef(var_model)

  paths_conditioned <- as.data.frame(paths_conditioned)

  if (nrow(paths_conditioned) == 0) {
    stop("`paths_conditioned` debe tener al menos una fila.")
  }

  if (is.null(h)) {
    h <- nrow(paths_conditioned)
  }
  h <- as.integer(h)
  if (h <= 0) stop("`h` debe ser un entero positivo.")

  if (nrow(paths_conditioned) > h) {
    paths_conditioned <- paths_conditioned[seq_len(h), , drop = FALSE]
  }

  n_cond <- nrow(paths_conditioned)
  conditioned_vars <- colnames(paths_conditioned)

  if (!all(conditioned_vars %in% var_names)) {
    stop("Algunas variables condicionadas no están en el VAR: ",
         paste(setdiff(conditioned_vars, var_names), collapse = ", "))
  }

  yhist <- if (is.null(ystart)) tail(as.matrix(var_model$y), p) else as.matrix(ystart)
  colnames(yhist) <- var_names

  ysim <- rbind(yhist, matrix(NA_real_, nrow = h, ncol = K))
  colnames(ysim) <- var_names

  baseline <- NULL
  if (fill_method == "baseline") {
    fc_base <- stats::predict(var_model, n.ahead = h, ci = 0.95)
    baseline <- sapply(fc_base$fcst, function(x) x[, "fcst"])
    colnames(baseline) <- var_names
  }

  for (step in seq_len(h)) {
    if (fill_method == "baseline" && step > n_cond) {
      y_pred <- baseline[step, ]
      names(y_pred) <- var_names
    } else {
      x <- rep(0, ncol(B)); names(x) <- colnames(B)
      for (lag in seq_len(p)) {
        for (v in var_names) {
          cname <- paste0(v, ".l", lag)
          if (cname %in% names(x)) x[cname] <- ysim[p + step - lag, v]
        }
      }
      if ("const" %in% names(x)) x["const"] <- 1
      y_pred <- as.numeric(B %*% x)
      names(y_pred) <- var_names
    }

    # Si el usuario solo condiciona parte del horizonte, las filas faltantes
    # se generan endógenamente con la dinámica del VAR. Además, valores NA
    # dentro de la senda condicionada se interpretan como "no imponer".
    if (step <= n_cond) {
      vals <- as.numeric(paths_conditioned[step, , drop = TRUE])
      names(vals) <- conditioned_vars
      vars_a_imponer <- names(vals)[!is.na(vals)]
      if (length(vars_a_imponer) > 0) {
        y_pred[vars_a_imponer] <- vals[vars_a_imponer]
      }
    }

    ysim[p + step, ] <- y_pred
  }

  out <- as.data.frame(ysim[(p + 1):(p + h), , drop = FALSE])
  out$horizonte <- seq_len(h)
  out
}

.default_severity <- function(path, sd_marg) {
  vars <- colnames(path)
  score <- 0
  if ("crecimiento" %in% vars) score <- score + sum(-path[, "crecimiento"] / sd_marg["crecimiento"])
  for (v in intersect(c("inflacion", "interbancaria", "deprec_fx", "embi"), vars)) {
    score <- score + sum(path[, v] / sd_marg[v])
  }
  score
}

.mat_to_long_var <- function(mat, escenario, enfoque, variables, T_total = 0) {
  df <- as.data.frame(mat)
  colnames(df) <- variables
  df$periodo <- T_total + seq_len(nrow(df))
  df |>
    tidyr::pivot_longer(cols = dplyr::all_of(variables),
                        names_to = "variable", values_to = "valor") |>
    dplyr::mutate(escenario = escenario, enfoque = enfoque)
}


.inferir_fechas_forecast <- function(fechas, h, frecuencia = c("auto", "mensual", "trimestral", "anual", "diaria")) {
  frecuencia <- match.arg(frecuencia)
  if (is.null(fechas)) return(NULL)

  fechas <- as.Date(fechas)
  fechas <- fechas[!is.na(fechas)]
  if (length(fechas) == 0) return(NULL)

  ultima_fecha <- tail(fechas, 1)

  if (frecuencia == "auto") {
    if (length(fechas) >= 2) {
      med_days <- stats::median(as.numeric(diff(fechas)), na.rm = TRUE)
    } else {
      med_days <- NA_real_
    }

    if (!is.na(med_days) && med_days >= 27 && med_days <= 32) {
      by <- "month"
    } else if (!is.na(med_days) && med_days >= 80 && med_days <= 100) {
      by <- "3 months"
    } else if (!is.na(med_days) && med_days >= 360 && med_days <= 370) {
      by <- "year"
    } else if (!is.na(med_days)) {
      return(ultima_fecha + round(med_days) * seq_len(h))
    } else {
      by <- "month"
    }
  } else if (frecuencia == "mensual") {
    by <- "month"
  } else if (frecuencia == "trimestral") {
    by <- "3 months"
  } else if (frecuencia == "anual") {
    by <- "year"
  } else {
    by <- "day"
  }

  seq.Date(from = ultima_fecha, by = by, length.out = h + 1)[-1]
}

#' Escenarios VAR por Monte Carlo, trayectoria conjunta extrema y VAR condicional
#'
#' Implementa tres enfoques seleccionables: percentiles marginales, trayectoria
#' conjunta extrema por índice de severidad y VAR condicional.
#'
#' @param modelo_var Modelo VAR estimado con `vars::VAR`.
#' @param h Horizonte.
#' @param M Número de simulaciones Monte Carlo.
#' @param percentiles Percentiles para el enfoque marginal. Usualmente c(0.5, 0.05, 0.01).
#' @param enfoques Vector con alguno de: "marginal", "conjunta", "condicional".
#' @param severity_index Función opcional: f(path, sd_marg) -> score. Mayor score = más adverso.
#' @param severity_by_path Vector opcional de longitud M con el índice de severidad precomputado por simulación.
#'   Si se pasa, reemplaza el cálculo interno con `severity_index` y permite al usuario definir
#'   severidad macro-previsional fuera de la función.
#' @param severity_probs Probabilidades de severidad para trayectoria conjunta. Ej.: c(0.5,0.95,0.99).
#' @param conjunta_resumen Forma de representar el conjunto severo: "path" selecciona una trayectoria individual,
#'   "media" promedia todas las trayectorias con severidad mayor o igual al cuantil indicado y
#'   "mediana" usa la mediana punto a punto del conjunto severo.
#' @param conditional_paths Lista opcional con sendas condicionadas, por ejemplo list(Baseline=df1, Adverso=df2).
#'   Cada data frame puede tener menos filas que `h`; las filas restantes se completan endógenamente.
#' @param fill_method Método para completar horizontes no condicionados: "recursive" usa la dinámica VAR
#'   después de los valores impuestos; "baseline" completa con la proyección baseline del VAR.
#' @param seed Semilla.
#' @param n_hist_plot Observaciones históricas a incluir en gráficos.
#' @param fechas Vector opcional de fechas observadas usadas para estimar el VAR. Si se pasa, el gráfico usa fechas reales.
#' @param frecuencia Frecuencia temporal para proyectar fechas futuras: "auto", "mensual", "trimestral", "anual" o "diaria".
#' @return Lista con `resultados`, `graficos`, `simulaciones`, `baseline`.
escenario_var_stress <- function(modelo_var,
                                 h = 8,
                                 M = 5000,
                                 percentiles = c(0.50, 0.05, 0.01),
                                 enfoques = c("marginal", "conjunta", "condicional"),
                                 severity_index = NULL,
                                 severity_by_path = NULL,
                                 severity_probs = c(0.50, 0.95, 0.99),
                                 conjunta_resumen = c("path", "media", "mediana"),
                                 conditional_paths = NULL,
                                 fill_method = c("recursive", "baseline"),
                                 seed = 20260501,
                                 n_hist_plot = 24,
                                 fechas = NULL,
                                 frecuencia = c("auto", "mensual", "trimestral", "anual", "diaria")) {
  enfoques <- match.arg(enfoques, choices = c("marginal", "conjunta", "condicional"), several.ok = TRUE)
  conjunta_resumen <- match.arg(conjunta_resumen)
  frecuencia <- match.arg(frecuencia)
  fill_method <- match.arg(fill_method)
  if (!is.null(seed)) set.seed(seed)

  Y <- as.matrix(modelo_var$y)
  variables <- colnames(Y)
  K <- ncol(Y)
  T_total <- nrow(Y)

  usar_fechas <- !is.null(fechas)
  if (usar_fechas) {
    fechas <- as.Date(fechas)
    if (length(fechas) < T_total) {
      stop("`fechas` debe tener al menos tantas observaciones como las usadas en el VAR.")
    }
    fechas_obs_full <- tail(fechas, T_total)
    fechas_fc <- .inferir_fechas_forecast(fechas_obs_full, h, frecuencia)
  } else {
    fechas_obs_full <- NULL
    fechas_fc <- NULL
  }

  fc <- stats::predict(modelo_var, n.ahead = h, ci = 0.95)
  base_var <- sapply(fc$fcst, function(x) x[, "fcst"])
  colnames(base_var) <- variables

  Sigma <- summary(modelo_var)$covres
  sim_paths <- array(NA_real_, dim = c(h, K, M),
                     dimnames = list(NULL, variables, NULL))
  for (m in seq_len(M)) {
    shocks <- MASS::mvrnorm(h, mu = rep(0, K), Sigma = Sigma)
    sim_paths[, , m] <- base_var + shocks
  }

  resultados_long <- list()
  resumen <- list()

  if ("marginal" %in% enfoques) {
    for (pctl in percentiles) {
      mat <- apply(sim_paths, c(1, 2), stats::quantile, probs = pctl, na.rm = TRUE)
      colnames(mat) <- variables
      etiqueta <- paste0("p", round(100 * pctl))
      resultados_long[[paste0("marginal_", etiqueta)]] <-
        .mat_to_long_var(mat, etiqueta, "Percentiles marginales", variables, T_total)
    }
  }

  if ("conjunta" %in% enfoques) {
    sd_marg <- apply(sim_paths, 2, stats::sd, na.rm = TRUE)

    if (!is.null(severity_by_path)) {
      if (!is.numeric(severity_by_path)) {
        stop("`severity_by_path` debe ser un vector numérico de longitud M.")
      }
      if (length(severity_by_path) != M) {
        stop("`severity_by_path` debe tener longitud M = ", M, ".")
      }
      scores <- as.numeric(severity_by_path)
    } else {
      sev_fun <- if (is.null(severity_index)) .default_severity else severity_index
      scores <- vapply(seq_len(M), function(m) sev_fun(sim_paths[, , m], sd_marg), numeric(1))
    }

    if (all(is.na(scores))) {
      stop("`severity_by_path` o `severity_index` produjo solo valores NA.")
    }

    resumen_conjunta <- list()
    for (prob in severity_probs) {
      q_prob <- stats::quantile(scores, prob, na.rm = TRUE)
      idx_set <- which(scores >= q_prob)

      if (length(idx_set) == 0) {
        idx_set <- which.max(scores)
      }

      if (conjunta_resumen == "path") {
        idx <- which.min(abs(scores - q_prob))
        mat <- sim_paths[, , idx, drop = FALSE][, , 1]
        metodo_resumen <- "path_individual"
      } else if (conjunta_resumen == "media") {
        mat <- apply(sim_paths[, , idx_set, drop = FALSE], c(1, 2), mean, na.rm = TRUE)
        metodo_resumen <- "media_conjunto_severo"
      } else {
        mat <- apply(sim_paths[, , idx_set, drop = FALSE], c(1, 2), stats::median, na.rm = TRUE)
        metodo_resumen <- "mediana_conjunto_severo"
      }

      colnames(mat) <- variables
      etiqueta <- paste0("severidad_p", round(100 * prob))
      resultados_long[[paste0("conjunta_", etiqueta)]] <-
        .mat_to_long_var(mat, etiqueta, "Trayectoria conjunta extrema", variables, T_total)

      resumen_conjunta[[etiqueta]] <- tibble::tibble(
        escenario = etiqueta,
        probabilidad_severidad = prob,
        cuantil_indice = as.numeric(q_prob),
        n_trayectorias_severas = length(idx_set),
        metodo_resumen = metodo_resumen
      )
    }
    resumen$conjunta_scores <- tibble::tibble(simulacion = seq_len(M), indice_severidad = scores)
    resumen$conjunta_resumen <- dplyr::bind_rows(resumen_conjunta)
  }

  if ("condicional" %in% enfoques) {
    if (is.null(conditional_paths)) {
      stop("Para usar enfoque condicional debe pasar `conditional_paths = list(nombre = data.frame(...))`.")
    }
    for (nm in names(conditional_paths)) {
      mat_df <- forecast_var_condicional(modelo_var, conditional_paths[[nm]], h = h, fill_method = fill_method)
      mat <- as.matrix(mat_df[, variables, drop = FALSE])
      resultados_long[[paste0("condicional_", nm)]] <-
        .mat_to_long_var(mat, nm, "VAR condicional", variables, T_total)
    }
  }

  hist_df <- utils::tail(as.data.frame(Y), n_hist_plot)
  hist_df$periodo <- (T_total - nrow(hist_df)) + seq_len(nrow(hist_df))
  if (usar_fechas) {
    hist_df$fecha <- utils::tail(fechas_obs_full, nrow(hist_df))
  }

  hist_long <- hist_df |>
    tidyr::pivot_longer(cols = dplyr::all_of(variables),
                        names_to = "variable", values_to = "valor") |>
    dplyr::mutate(escenario = "Histórico", enfoque = "Histórico")

  baseline_long <- .mat_to_long_var(base_var, "Baseline VAR", "Baseline", variables, T_total)
  resultados <- dplyr::bind_rows(resultados_long)
  resultados <- dplyr::bind_rows(baseline_long, resultados)

  if (usar_fechas) {
    mapa_fechas_fc <- tibble::tibble(
      periodo = T_total + seq_len(h),
      fecha = fechas_fc
    )
    resultados <- resultados |>
      dplyr::left_join(mapa_fechas_fc, by = "periodo")
  }

  # Para visualización regulatoria y docente, las trayectorias futuras se
  # anclan en el último dato observado. Esto evita paneles desconectados y
  # muestra la transición correcta: histórico -> baseline/adversos.
  ultimo_hist <- hist_long |>
    dplyr::filter(.data$periodo == T_total) |>
    dplyr::select(dplyr::any_of(c("variable", "periodo", "fecha", "valor")))

  escenarios_futuros <- unique(resultados$escenario)
  anclas <- tidyr::crossing(
      ultimo_hist,
      escenario = escenarios_futuros
    ) |>
    dplyr::mutate(enfoque = "Punto de inicio")

  plot_data <- dplyr::bind_rows(
    hist_long,
    anclas,
    resultados
  ) |>
    dplyr::mutate(
      escenario = factor(
        .data$escenario,
        levels = unique(c("Histórico", "Baseline VAR", setdiff(as.character(.data$escenario), c("Histórico", "Baseline VAR"))))
      )
    )

  x_col <- if (usar_fechas) "fecha" else "periodo"
  x_label <- if (usar_fechas) {
    if (frecuencia == "trimestral") "Fecha (trimestres)" else if (frecuencia == "anual") "Fecha (años)" else if (frecuencia == "mensual") "Fecha (meses)" else "Fecha"
  } else {
    "Periodo"
  }
  x_intercept <- if (usar_fechas) as.numeric(utils::tail(fechas_obs_full, 1)) else T_total

  grafico_panel <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data[[x_col]],
        y = .data$valor,
        color = .data$escenario,
        group = interaction(.data$variable, .data$escenario)
      )
    ) +
    ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = x_intercept, linetype = "dashed") +
    ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 11),
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = "Escenarios VAR para stress testing previsional",
      subtitle = "Histórico, baseline y escenarios adversos conectados en el horizonte de proyección",
      x = x_label,
      y = "Valor",
      color = "Escenario"
    )

  if (usar_fechas) {
    grafico_panel <- grafico_panel +
      ggplot2::scale_x_date(date_breaks = if (frecuencia == "mensual") "6 months" else if (frecuencia == "trimestral") "1 year" else "2 years",
                            date_labels = if (frecuencia == "mensual") "%Y-%m" else "%Y")
  }

  list(
    resultados = resultados,
    datos_grafico = plot_data,
    graficos = list(panel = grafico_panel),
    simulaciones = sim_paths,
    baseline = as.data.frame(base_var),
    resumen = resumen,
    forecast = fc
  )
}
