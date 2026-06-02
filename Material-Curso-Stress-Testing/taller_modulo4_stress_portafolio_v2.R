
# Taller Integrador Módulo 4 — Stress Testing del Portafolio Previsional
# MACROPOL: Economía Aplicada y Políticas Públicas
# Entradas:
#   1. datos_macro_pensiones.csv
#   2. rendimientos_por_fondo.csv
#   3. tabla_escenarios_5_hist_mas_proyecciones.csv
# Salidas:
#   ./salidas_modulo4/betas_factoriales.csv
#   ./salidas_modulo4/diagnostico_garch.csv
#   ./salidas_modulo4/resultados_var_es.csv
#   ./salidas_modulo4/perdidas_simuladas.csv
rm(list=ls())
paquetes <- c("tidyverse", "lubridate", "rugarch", "broom", "purrr")
instalar <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(instalar) > 0) install.packages(instalar)
invisible(lapply(paquetes, library, character.only = TRUE))

set.seed(1234)

setwd(r'(C:\Users\t490\Documents\MacroPol\stress-testing\data)')

#dir.create("salidas_modulo4", showWarnings = FALSE)

# -------------------------------------------------------------------------
# 1. Carga y preparación de datos
# -------------------------------------------------------------------------

rend <- readr::read_csv("rendimientos_por_fondo.csv", show_col_types = FALSE)
macro <- readr::read_csv("datos_macro_pensiones.csv", show_col_types = FALSE)
escenarios <- readr::read_csv("tabla_escenarios.csv", show_col_types = FALSE)

# Transformación macro:
# - crecimiento: variación interanual del IMAE
# - inflación: variación interanual del IPC
# - deprec_fx: depreciación interanual del tipo de cambio
# - embi e interbancaria: se usan en niveles
macro_f <- macro %>%
  mutate(
    fecha = lubridate::mdy(date),
    year = lubridate::year(fecha),
    mes = lubridate::month(fecha),
    crecimiento = 100 * (log(imae) - log(dplyr::lag(imae, 12))),
    inflacion = 100 * (log(ipc) - log(dplyr::lag(ipc, 12))),
    deprec_fx = 100 * (log(tipo_de_cambio) - log(dplyr::lag(tipo_de_cambio, 12))),
    embi = embi,
    interbancaria = interbancaria
  ) %>%
  dplyr::select(fecha, year, mes, crecimiento, inflacion, deprec_fx, embi, interbancaria)

base_modelo <- rend %>%
  mutate(
    fecha = lubridate::make_date(year, mes, 1),
    # rate está en porcentaje anual. Para la regresión se usa escala decimal anual.
    # Para rentabilidad mensual acumulada puede cambiarse a rate / 100 / 12.
    ret_modelo = rate / 100
  ) %>%
  left_join(macro_f, by = c("year", "mes")) %>%
  drop_na(ret_modelo, crecimiento, inflacion, deprec_fx, embi, interbancaria)

# -------------------------------------------------------------------------
# 2. Modelo multifactorial por fondo e instrumento
# -------------------------------------------------------------------------

formula_factores <- ret_modelo ~ crecimiento + inflacion + deprec_fx + embi + interbancaria

estimar_lm <- function(df) {
  fit <- lm(formula_factores, data = df)
  tibble(
    alpha = coef(fit)[["(Intercept)"]],
    beta_crecimiento = coef(fit)[["crecimiento"]],
    beta_inflacion = coef(fit)[["inflacion"]],
    beta_deprec_fx = coef(fit)[["deprec_fx"]],
    beta_embi = coef(fit)[["embi"]],
    beta_interbancaria = coef(fit)[["interbancaria"]],
    r2 = summary(fit)$r.squared,
    n = nobs(fit)
  )
}

betas <- base_modelo %>%
  group_by(fund, instrumento) %>%
  filter(n() >= 36) %>%
  group_modify(~ estimar_lm(.x)) %>%
  ungroup()

residuos <- base_modelo %>%
  semi_join(betas %>% select(fund, instrumento), by = c("fund", "instrumento")) %>%
  group_by(fund, instrumento) %>%
  group_modify(~ {
    fit <- lm(formula_factores, data = .x)
    broom::augment(fit, data = .x) %>%
      transmute(
        fecha, year, mes, ret_modelo,
        crecimiento, inflacion, deprec_fx, embi, interbancaria,
        ajustado = .fitted,
        residuo = .resid
      )
  }) %>%
  ungroup()

readr::write_csv(betas, "salidas_modulo4/betas_factoriales.csv")

# -------------------------------------------------------------------------
# 3. GARCH(1,1) sobre residuos del modelo multifactorial
# -------------------------------------------------------------------------

spec_garch <- rugarch::ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "std"
)

ajustar_garch <- function(e) {
  e <- as.numeric(e)
  e <- e[is.finite(e)]
  if (length(e) < 36 || sd(e) == 0) return(NULL)
  tryCatch(
    rugarch::ugarchfit(spec = spec_garch, data = e, solver = "hybrid"),
    error = function(err) NULL
  )
}

diag_garch <- residuos %>%
  group_by(fund, instrumento) %>%
  summarise(
    n = n(),
    sd_resid = sd(residuo, na.rm = TRUE),
    fit = list(ajustar_garch(residuo)),
    .groups = "drop"
  ) %>%
  mutate(
    convergencia = purrr::map_int(fit, ~ if (is.null(.x)) 1L else .x@fit$convergence),
    omega = purrr::map_dbl(fit, ~ if (is.null(.x)) NA_real_ else coef(.x)[["omega"]]),
    alpha1 = purrr::map_dbl(fit, ~ if (is.null(.x)) NA_real_ else coef(.x)[["alpha1"]]),
    beta1 = purrr::map_dbl(fit, ~ if (is.null(.x)) NA_real_ else coef(.x)[["beta1"]]),
    shape = purrr::map_dbl(fit, ~ if (is.null(.x)) NA_real_ else coef(.x)[["shape"]]),
    sigma_ultimo = purrr::map_dbl(fit, ~ {
      if (is.null(.x)) NA_real_ else as.numeric(tail(sigma(.x), 1))
    }),
    persistencia = alpha1 + beta1,
    sigma_sim = if_else(is.na(sigma_ultimo) | sigma_ultimo <= 0, sd_resid, sigma_ultimo)
  ) %>%
  select(-fit)

readr::write_csv(diag_garch, "salidas_modulo4/diagnostico_garch.csv")

# -------------------------------------------------------------------------
# 4. Escenarios del Módulo 3 y simulación Monte Carlo
# -------------------------------------------------------------------------

esc_f <- escenarios %>%
  mutate(
    fecha = lubridate::ym(periodo),
    escenario = as.character(escenario)
  ) %>%
  select(fecha, escenario, crecimiento, deprec_fx, embi, inflacion, interbancaria) %>%
  drop_na()

# Supuesto pedagógico de pesos:
# Si no hay archivo de saldos/tenencias, se usan pesos iguales por instrumento dentro de cada fondo.
pesos <- base_modelo %>%
  distinct(fund, instrumento) %>%
  group_by(fund) %>%
  mutate(peso = 1 / n()) %>%
  ungroup()

parametros <- betas %>%
  left_join(diag_garch, by = c("fund", "instrumento")) %>%
  left_join(pesos, by = c("fund", "instrumento")) %>%
  mutate(
    sigma_sim = if_else(is.na(sigma_sim) | sigma_sim <= 0, 0.01, sigma_sim),
    shape = if_else(is.na(shape) | shape <= 2, 8, shape)
  )

simular_un_fondo <- function(fondo_id, escenario_id, n_sim = 10000, alpha_var = 0.99) {
  pars <- parametros %>% filter(fund == fondo_id)
  esc <- esc_f %>% filter(escenario == escenario_id)
  if (nrow(pars) == 0 || nrow(esc) == 0) return(tibble())

  sim <- map_dfr(seq_len(n_sim), function(s) {
    ret_t <- map_dbl(seq_len(nrow(esc)), function(j) {
      x <- esc[j, ]
      ret_assets <- pars %>%
        mutate(
          media = alpha +
            beta_crecimiento * x$crecimiento +
            beta_inflacion * x$inflacion +
            beta_deprec_fx * x$deprec_fx +
            beta_embi * x$embi +
            beta_interbancaria * x$interbancaria,
          shock = sigma_sim * rt(n(), df = shape),
          ret_sim = media + shock
        )
      sum(ret_assets$peso * ret_assets$ret_sim, na.rm = TRUE)
    })
    tibble(
      fund = fondo_id,
      escenario = escenario_id,
      simulacion = s,
      retorno_acumulado = prod(1 + ret_t) - 1,
      perdida = pmax(-(prod(1 + ret_t) - 1), 0)
    )
  })

  sim
}

fondos <- unique(parametros$fund)
escs <- unique(esc_f$escenario)
n_sim <- 10000

perdidas_simuladas <- tidyr::crossing(fund = fondos, escenario = escs) %>%
  mutate(data = purrr::map2(fund, escenario, ~ simular_un_fondo(.x, .y, n_sim = n_sim))) %>%
  select(data) %>%
  unnest(data)

resultados_riesgo <- perdidas_simuladas %>%
  group_by(fund, escenario) %>%
  summarise(
    simulaciones = n(),
    retorno_mediano = median(retorno_acumulado, na.rm = TRUE),
    perdida_esperada_promedio = mean(perdida, na.rm = TRUE),
    VaR_95 = quantile(perdida, 0.95, na.rm = TRUE),
    ES_95 = mean(perdida[perdida >= quantile(perdida, 0.95, na.rm = TRUE)], na.rm = TRUE),
    VaR_99 = quantile(perdida, 0.99, na.rm = TRUE),
    ES_99 = mean(perdida[perdida >= quantile(perdida, 0.99, na.rm = TRUE)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(escenario, desc(ES_99))

readr::write_csv(perdidas_simuladas, "salidas_modulo4/perdidas_simuladas.csv")
readr::write_csv(resultados_riesgo, "salidas_modulo4/resultados_var_es.csv")

# -------------------------------------------------------------------------
# 5. Lectura supervisora
# -------------------------------------------------------------------------

print("Resumen VaR / Expected Shortfall por fondo y escenario:")
print(resultados_riesgo)

print("Archivos generados en ./salidas_modulo4")
