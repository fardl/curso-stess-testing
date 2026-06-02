#' Escenario adverso ARIMA con perfil de shock k_h
#'
#' La función toma un modelo ARIMA ya estimado y construye un escenario baseline
#' con `forecast::forecast`. El escenario adverso se define como baseline más
#' k_h veces la desviación estándar de los residuos del modelo. Opcionalmente
#' agrega al gráfico un tramo histórico observado y fechas reales en el eje x.
#'
#' @param modelo_arima Modelo estimado compatible con `forecast::forecast`.
#' @param k_h Vector con el perfil temporal del shock. Su longitud define h, salvo que se indique `h`.
#' @param h Horizonte de proyección. Por defecto `length(k_h)`.
#' @param nombre_variable Etiqueta de la variable graficada.
#' @param tasa_nominal Vector opcional para computar tasa real baseline/adversa.
#' @param sigma Shock unitario. Si es NULL, usa sd(residuals(modelo_arima)).
#' @param nivel_confianza Nivel de intervalo para forecast.
#' @param n_hist Número de observaciones históricas que se agregan al gráfico. Si es NULL o 0, no agrega historia.
#' @param fechas Vector opcional de fechas de la serie observada usada para estimar el ARIMA.
#'   Puede ser Date, POSIXct o valores convertibles a Date. Si se pasa, el eje x usa fechas reales.
#' @return Lista con `escenarios`, `historico`, `datos_grafico`, `grafico` y `forecast`.
escenario_arima_shock <- function(modelo_arima,
                                  k_h,
                                  h = length(k_h),
                                  nombre_variable = "variable",
                                  tasa_nominal = NULL,
                                  sigma = NULL,
                                  nivel_confianza = 95,
                                  n_hist = 0,
                                  fechas = NULL) {
  if (length(k_h) < h) stop("k_h debe tener al menos h elementos.")
  k_h <- as.numeric(k_h[seq_len(h)])

  fc <- forecast::forecast(modelo_arima, h = h, level = nivel_confianza)
  baseline <- as.numeric(fc$mean)

  if (is.null(sigma)) sigma <- stats::sd(stats::residuals(modelo_arima), na.rm = TRUE)
  shock <- k_h * sigma
  adverso <- baseline + shock

  serie_obs <- NULL
  if (!is.null(modelo_arima$x)) {
    serie_obs <- as.numeric(modelo_arima$x)
  } else if (!is.null(modelo_arima$series) && exists(modelo_arima$series, envir = parent.frame())) {
    serie_obs <- as.numeric(get(modelo_arima$series, envir = parent.frame()))
  } else {
    resid <- stats::residuals(modelo_arima)
    fitted <- stats::fitted(modelo_arima)
    if (!is.null(fitted) && !is.null(resid) && length(fitted) == length(resid)) {
      serie_obs <- as.numeric(fitted + resid)
    }
  }

  T_obs <- if (!is.null(serie_obs)) length(serie_obs) else 0

  usar_fechas <- !is.null(fechas)
  fechas_obs <- NULL
  fechas_fc <- NULL

  if (usar_fechas) {
    if (length(fechas) < T_obs && T_obs > 0) {
      stop("`fechas` debe tener al menos tantas observaciones como la serie usada en el ARIMA.")
    }

    if (inherits(fechas, "Date")) {
      fechas_obs <- fechas
    } else {
      fechas_obs <- as.Date(fechas)
    }

    if (any(is.na(fechas_obs))) {
      stop("`fechas` contiene valores que no pueden convertirse a Date.")
    }

    fechas_obs <- fechas_obs[seq_len(length(fechas_obs))]
    if (T_obs > 0) fechas_obs <- tail(fechas_obs, T_obs)

    if (length(fechas_obs) >= 2) {
      diffs <- diff(fechas_obs)
      med_days <- as.numeric(stats::median(diffs, na.rm = TRUE))
    } else {
      med_days <- 30
    }

    ultima_fecha <- tail(fechas_obs, 1)
    
    if (!is.na(med_days) && med_days >= 27 && med_days <= 32) {
      fechas_fc <- seq.Date(
        from = as.Date(ultima_fecha),
        by = "month",
        length.out = h + 1
      )[-1]
      
    } else if (!is.na(med_days) && med_days >= 80 && med_days <= 100) {
      fechas_fc <- seq.Date(
        from = as.Date(ultima_fecha),
        by = "3 months",
        length.out = h + 1
      )[-1]
      
    } else if (!is.na(med_days) && med_days >= 360 && med_days <= 370) {
      fechas_fc <- seq.Date(
        from = as.Date(ultima_fecha),
        by = "year",
        length.out = h + 1
      )[-1]
      
    } else {
      fechas_fc <- as.Date(ultima_fecha) + round(med_days) * seq_len(h)
    }
  }

  escenarios <- tibble::tibble(
    horizonte = seq_len(h),
    variable = nombre_variable,
    baseline = baseline,
    k_h = k_h,
    sigma = sigma,
    shock = shock,
    adverso = adverso
  )

  if (usar_fechas) {
    escenarios$fecha <- fechas_fc
  } else {
    escenarios$periodo <- T_obs + seq_len(h)
  }

  if (!is.null(tasa_nominal)) {
    if (length(tasa_nominal) == 1) tasa_nominal <- rep(tasa_nominal, h)
    if (length(tasa_nominal) < h) stop("tasa_nominal debe tener longitud 1 o al menos h.")
    escenarios$tasa_nominal <- as.numeric(tasa_nominal[seq_len(h)])
    escenarios$tasa_real_baseline <- escenarios$tasa_nominal - escenarios$baseline
    escenarios$tasa_real_adversa <- escenarios$tasa_nominal - escenarios$adverso
  }

  historico <- NULL
  if (!is.null(n_hist) && n_hist > 0) {
    if (is.null(serie_obs)) {
      warning("No se pudo extraer la serie observada del modelo ARIMA; el gráfico se hará sin historia.")
    } else {
      n_hist_eff <- min(as.integer(n_hist), length(serie_obs))
      historico <- tibble::tibble(
        variable = nombre_variable,
        valor = tail(serie_obs, n_hist_eff),
        escenario = "historico"
      )
      if (usar_fechas) {
        historico$fecha <- tail(fechas_obs, n_hist_eff)
      } else {
        historico$periodo <- (T_obs - n_hist_eff + 1):T_obs
      }
    }
  }

  plot_fc <- escenarios |>
    dplyr::select(dplyr::any_of(c("fecha", "periodo", "horizonte")),
                  .data$baseline, .data$adverso) |>
    tidyr::pivot_longer(cols = c("baseline", "adverso"),
                        names_to = "escenario",
                        values_to = "valor")

  # Ancla los escenarios futuros en el último dato observado para eliminar
  # el espacio visual entre historia y pronóstico.
  if (!is.null(historico)) {
    ultimo <- utils::tail(historico, 1)
    anclas <- tibble::tibble(
      escenario = c("baseline", "adverso"),
      valor = as.numeric(ultimo$valor)
    )
    if (usar_fechas) {
      anclas$fecha <- ultimo$fecha
    } else {
      anclas$periodo <- ultimo$periodo
    }
    plot_df <- dplyr::bind_rows(historico, anclas, plot_fc)
  } else {
    plot_df <- plot_fc
  }

  xvar <- if (usar_fechas) "fecha" else if ("periodo" %in% names(plot_df)) "periodo" else "horizonte"

  grafico <- ggplot2::ggplot(
      plot_df,
      ggplot2::aes(x = .data[[xvar]], y = .data$valor,
                   color = .data$escenario,
                   group = .data$escenario)
    ) +
    ggplot2::geom_line(linewidth = 1.1, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.8, na.rm = TRUE) +
    ggplot2::labs(
      title = paste0(nombre_variable, ": baseline vs escenario adverso"),
      subtitle = "Adverso = baseline + k_h * sigma(residuos ARIMA)",
      x = if (usar_fechas) "Fecha" else "Periodo",
      y = nombre_variable,
      color = "Escenario"
    ) +
    ggplot2::theme_minimal()

  if (!is.null(historico)) {
    x_inicio <- if (usar_fechas) utils::tail(historico$fecha, 1) else utils::tail(historico$periodo, 1)
    grafico <- grafico + ggplot2::geom_vline(xintercept = x_inicio, linetype = "dashed")
  }

  list(
    escenarios = escenarios,
    historico = historico,
    datos_grafico = plot_df,
    grafico = grafico,
    forecast = fc
  )
}
