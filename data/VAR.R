#################################################
# CURSO:
# Stress Testing en Sistemas de Pensiones
# MACROPOL
#
# EJEMPLO:
# Uso del modelo VAR
#################################################

#------------------------------------------------
# 0. Borrar todo
#------------------------------------------------
# 0.1. Limpiar objetos
rm(list=ls())

# 0.2. Cerrar gráficos
graphics.off()
# 0.3. detach paquetes
pkgs <- names(sessionInfo()$otherPkgs)

if(length(pkgs) > 0){
  for(p in rev(pkgs)){
    try(
      detach(
        paste0("package:", p),
        character.only = TRUE,
        unload = TRUE
      ),
      silent = TRUE
    )
  }
}

# 0.4. Limpiar consola
cat("\014")
gc()

#-----------------------------------------------
# 1. Librerias
#-----------------------------------------------
library(vars)
library(ggplot2)
library(tidyr)
library(dplyr)

#-----------------------------------------------
# 2. Cargar datos
#-----------------------------------------------
setwd(r'(C:\Users\t490\Documents\MacroPol\stress-testing\data)') # <--- Aqui debes poner la ruta de tu PC donde guardas los datos
source("plantilla_datos.R")

Y <- datos_modelo |>
  dplyr::select(embi, crecimiento, inflacion, interbancaria,
           deprec_fx) |>
  as.matrix()


#-----------------------------------------------
# 3. Seleccion del numero de rezagos del VAR
#-----------------------------------------------
VARselect(Y, lag.max = 4, type="const")

#-----------------------------------------------
# 4. Estimacion 
#-----------------------------------------------
fit_var <- VAR(Y, p=2, type="const")
summary(fit_var)

#-----------------------------------------------
# 5. Diagnotisco
#-----------------------------------------------

# 5.1. Contraste de autocorrelacion
serial.test(fit_var, lags.pt = 4, type="PT.asymptotic")

# 5.2. Contraste de heterocedasticidad
arch.test(fit_var, lags.multi = 4)

# 5.3. Verificacion estabilidad del VAR
roots(fit_var) 

#-----------------------------------------------
# 6. Analisis Impulso Respuesta
#-----------------------------------------------

# 6.1. Respuesta ante un shock monetario
irf_fx <- irf(fit_var,
              impulse = "interbancaria",
              response = c("inflacion", "interbancaria",
                           "crecimiento", "deprec_fx"),
              n.ahead = 24,
              boot = TRUE,
              ci = 0.90)

plot(irf_fx)

#-----------------------------------------------
# 7. Pronosticos (escenario base) multivariado
#-----------------------------------------------
fc_var <- predict(fit_var, n.ahead=24, ci=0.95)

# Extraer trayectoria central
base_var <- sapply(fc_var$fcst, function(x) x[,"fcst"])
base_var

#-----------------------------------------------
# 8. Graficar histórico + Pronósticos VAR
#-----------------------------------------------

#-----------------------------------------------
# 8.1. Datos históricos
#-----------------------------------------------

hist <- datos_modelo |>
  dplyr::select(
    fecha,
    embi,
    crecimiento,
    inflacion,
    interbancaria,
    deprec_fx
  )

n_hist <- 20

hist_tail <- tail(hist, n_hist)

#-----------------------------------------------
# 8.2. Detectar frecuencia temporal
#-----------------------------------------------

med_days <- median(diff(hist_tail$fecha))

ultima_fecha <- max(hist_tail$fecha)

h <- nrow(base_var)

if (med_days >= 27 && med_days <= 32) {
  
  fechas_fc <- seq.Date(
    from = ultima_fecha,
    by = "month",
    length.out = h + 1
  )[-1]
  
} else if (med_days >= 80 && med_days <= 100) {
  
  fechas_fc <- seq.Date(
    from = ultima_fecha,
    by = "quarter",
    length.out = h + 1
  )[-1]
  
} else if (med_days >= 360 && med_days <= 370) {
  
  fechas_fc <- seq.Date(
    from = ultima_fecha,
    by = "year",
    length.out = h + 1
  )[-1]
  
} else {
  
  fechas_fc <- ultima_fecha +
    round(med_days) * seq_len(h)
  
}

#-----------------------------------------------
# 8.3. Forecast baseline
#-----------------------------------------------

fc_df <- as.data.frame(base_var) |>
  mutate(
    fecha = fechas_fc
  )

#-----------------------------------------------
# 8.4. Marcar histórico
#-----------------------------------------------

hist_df <- hist_tail |>
  mutate(tipo = "Histórico")

#-----------------------------------------------
# 8.5. Marcar forecast
#-----------------------------------------------

fc_df <- fc_df |>
  mutate(tipo = "Forecast")

#-----------------------------------------------
# 8.6. Unir histórico + forecast
#-----------------------------------------------

hist_fc <- bind_rows(hist_df, fc_df)

#-----------------------------------------------
# 8.7. Formato largo
#-----------------------------------------------

hist_fc_long <- hist_fc |>
  pivot_longer(
    cols = c(
      embi,
      crecimiento,
      inflacion,
      interbancaria,
      deprec_fx
    ),
    names_to = "variable",
    values_to = "valor"
  )

#-----------------------------------------------
# 8.8. Gráfico
#-----------------------------------------------

hist_fc_long <- hist_fc_long |>
  mutate(grupo_linea = 1)

ggplot(
  hist_fc_long,
  aes(
    x = fecha,
    y = valor
  )
) +
  
  geom_line(
    aes(
      color = tipo,
      group = grupo_linea
    ),
    linewidth = 1.1
  ) +
  
  geom_vline(
    xintercept = ultima_fecha,
    linetype = "dashed"
  ) +
  
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_color_manual(
    values = c(
      "Histórico" = "black",
      "Forecast" = "blue"
    )
  ) +
  
  labs(
    title = "Escenario baseline VAR",
    subtitle = "Línea vertical = inicio del forecast",
    x = NULL,
    y = "Valor",
    color = NULL
  ) +
  
  theme_minimal(base_size = 13)
