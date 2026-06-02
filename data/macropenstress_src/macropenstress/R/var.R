
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

#' Forecast VAR condicionado a sendas exógenamente impuestas
#'
#' Impone valores de una o más variables en cada horizonte y propaga el resto
#' por la dinámica del VAR estimado.
#'
#' @param var_model Modelo VAR estimado con `vars::VAR`.
#' @param paths_conditioned Data frame o matriz H x q con columnas de variables condicionadas.
#' @param ystart Historia inicial opcional con p filas y K columnas.
#' @return Data frame H x K más columna `horizonte`.
forecast_var_condicional <- function(var_model, paths_conditioned, ystart = NULL) {
  K <- var_model$K
  p <- var_model$p
  var_names <- colnames(var_model$y)
  B <- vars::Bcoef(var_model)
  paths_conditioned <- as.data.frame(paths_conditioned)
  h <- nrow(paths_conditioned)
  conditioned_vars <- colnames(paths_conditioned)

  if (!all(conditioned_vars %in% var_names)) {
    stop("Algunas variables condicionadas no están en el VAR: ",
         paste(setdiff(conditioned_vars, var_names), collapse = ", "))
  }

  yhist <- if (is.null(ystart)) tail(as.matrix(var_model$y), p) else as.matrix(ystart)
  colnames(yhist) <- var_names

  ysim <- rbind(yhist, matrix(NA_real_, nrow = h, ncol = K))
  colnames(ysim) <- var_names

  for (step in seq_len(h)) {
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
    y_pred[conditioned_vars] <- as.numeric(paths_conditioned[step, ])
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
#' @param seed Semilla.
#' @param n_hist_plot Observaciones históricas a incluir en gráficos.
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
                                 seed = 20260501,
                                 n_hist_plot = 24) {
  enfoques <- match.arg(enfoques, choices = c("marginal", "conjunta", "condicional"), several.ok = TRUE)
  conjunta_resumen <- match.arg(conjunta_resumen)
  if (!is.null(seed)) set.seed(seed)

  Y <- as.matrix(modelo_var$y)
  variables <- colnames(Y)
  K <- ncol(Y)
  T_total <- nrow(Y)

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
      mat_df <- forecast_var_condicional(modelo_var, conditional_paths[[nm]])
      mat <- as.matrix(mat_df[, variables, drop = FALSE])
      resultados_long[[paste0("condicional_", nm)]] <-
        .mat_to_long_var(mat, nm, "VAR condicional", variables, T_total)
    }
  }

  hist_df <- utils::tail(as.data.frame(Y), n_hist_plot)
  hist_df$periodo <- (T_total - nrow(hist_df)) + seq_len(nrow(hist_df))
  hist_long <- hist_df |>
    tidyr::pivot_longer(cols = dplyr::all_of(variables),
                        names_to = "variable", values_to = "valor") |>
    dplyr::mutate(escenario = "Histórico", enfoque = "Histórico")

  baseline_long <- .mat_to_long_var(base_var, "Baseline VAR", "Baseline", variables, T_total)
  resultados <- dplyr::bind_rows(resultados_long)
  resultados <- dplyr::bind_rows(baseline_long, resultados)

  # Para visualización regulatoria y docente, las trayectorias futuras se
  # anclan en el último dato observado. Esto evita paneles desconectados y
  # muestra la transición correcta: histórico -> baseline/adversos.
  ultimo_hist <- hist_long |>
    dplyr::filter(.data$periodo == T_total) |>
    dplyr::select("variable", "periodo", "valor")

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

  grafico_panel <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$periodo,
        y = .data$valor,
        color = .data$escenario,
        group = interaction(.data$variable, .data$escenario)
      )
    ) +
    ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_vline(xintercept = T_total, linetype = "dashed") +
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
      x = "Periodo",
      y = "Valor",
      color = "Escenario"
    )

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
