library(tidyverse)
rm(list=ls())
# ==============================
# 1. PARAMETROS DEL MODELO
# ==============================

edad_inicio <- 20
edad_retiro <- 60
edad_muerte <- 85

salario_inicial <- 30000          # salario mensual inicial RD$
crec_salario <- 0.02              # crecimiento real del salario
cotizacion <- 0.084               # porcentaje al fondo
rendimiento <- 0.04               # retorno real del fondo
tasa_tecnica <- 0.03              # tasa técnica
meses <- (edad_retiro - edad_inicio) * 12

# ==============================
# 2. SIMULACION DE VIDA LABORAL
# ==============================

edad <- rep(seq(edad_inicio, edad_retiro - 1/12, by = 1/12), length.out = meses)

salario <- salario_inicial * (1 + crec_salario)^(edad - edad_inicio)

aporte <- salario * cotizacion

fondo <- numeric(length(edad))

for(i in 2:length(fondo)){
  fondo[i] <- fondo[i-1]*(1+rendimiento/12) + aporte[i]
}

capital_final <- tail(fondo,1)

# ==============================
# 3. CALCULO DE PENSION
# ==============================

años_retiro <- edad_muerte - edad_retiro
n <- años_retiro * 12

anualidad <- sum(1/(1+tasa_tecnica/12)^(1:n))

pension_mensual <- capital_final / anualidad

# ==============================
# 4. FLUJO DE PENSION
# ==============================

edad_pension <- seq(edad_retiro, edad_muerte, by = 1/12)[1:n]

pension <- rep(pension_mensual, n)

df_pension <- data.frame(
  edad = edad_pension,
  pension = pension
)

# ==============================
# 5. GRAFICO
# ==============================

ggplot(df_pension, aes(x=edad, y=pension)) +
  geom_line(size=1.2) +
  labs(
    title="Simulación de flujo de pensión",
    subtitle=paste("Capital acumulado:", round(capital_final)),
    x="Edad",
    y="Pensión mensual (RD$)"
  ) +
  theme_minimal()

