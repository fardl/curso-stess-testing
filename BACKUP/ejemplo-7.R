library(tidyverse)

# =========================
# PARAMETROS
# =========================

edad_inicio <- 20
edad_retiro <- 60
edad_muerte <- 85

salario_inicial <- 30000

crec_real_salario <- 0.01
inflacion <- 0.04

cotizacion <- 0.084

rendimiento_real <- 0.04
tasa_tecnica_real <- 0.03

# conversion a tasas nominales (Fisher)
rendimiento_nominal <- (1+rendimiento_real)*(1+inflacion)-1
tasa_tecnica_nominal <- (1+tasa_tecnica_real)*(1+inflacion)-1

meses_trabajo <- (edad_retiro-edad_inicio)*12
meses_retiro <- (edad_muerte-edad_retiro)*12

# =========================
# ACUMULACION DEL FONDO
# =========================

edad <- seq(edad_inicio, edad_retiro-1/12, by=1/12)

crec_nominal_salario <- (1+crec_real_salario)*(1+inflacion)-1

salario <- salario_inicial*(1+crec_nominal_salario)^(edad-edad_inicio)

aporte <- salario*cotizacion

fondo <- numeric(length(edad))

for(i in 2:length(fondo)){
  fondo[i] <- fondo[i-1]*(1+rendimiento_nominal/12) + aporte[i]
}

capital <- tail(fondo,1)

cat("Capital acumulado al retiro:", round(capital), "\n")

# =========================
# RETIRO PROGRAMADO NOMINAL
# =========================

edad_pension <- seq(edad_retiro, edad_muerte-1/12, by=1/12)

pension_rp <- numeric(meses_retiro)
fondo_rp <- numeric(meses_retiro)

fondo_rp[1] <- capital

for(t in 1:meses_retiro){
  
  # recalculo anual
  
  if(t==1 | (t-1)%%12==0){
    
    vida_restante <- meses_retiro - t + 1
    
    anualidad <- sum(1/(1+tasa_tecnica_nominal/12)^(1:vida_restante))
    
    pension_actual <- fondo_rp[t]/anualidad
    
  }
  
  pension_rp[t] <- pension_actual
  
  if(t < meses_retiro){
    
    fondo_rp[t+1] <- fondo_rp[t]*(1+rendimiento_nominal/12) - pension_rp[t]
    
  }
}

# =========================
# RENTA VITALICIA NOMINAL
# =========================

anualidad_total <- sum(1/(1+tasa_tecnica_nominal/12)^(1:meses_retiro))

pension_rv <- rep(capital/anualidad_total, meses_retiro)

# =========================
# CALCULO VALORES REALES
# =========================

inflacion_acum <- (1+inflacion/12)^(0:(meses_retiro-1))

pension_rp_real <- pension_rp / inflacion_acum
pension_rv_real <- pension_rv / inflacion_acum

# =========================
# DATAFRAME
# =========================

df <- data.frame(
  
  edad = edad_pension,
  
  retiro_programado_nominal = pension_rp,
  retiro_programado_real = pension_rp_real,
  
  renta_vitalicia_nominal = pension_rv,
  renta_vitalicia_real = pension_rv_real
  
)

# =========================
# GRAFICO NOMINAL
# =========================

ggplot(df, aes(x=edad)) +
  
  geom_line(aes(y=retiro_programado_nominal, color="Retiro programado")) +
  
  geom_line(aes(y=renta_vitalicia_nominal, color="Renta vitalicia")) +
  
  labs(
    title="Pensión nominal por modalidad",
    x="Edad",
    y="Pensión mensual (pesos corrientes)",
    color="Modalidad"
  ) +
  
  theme_minimal()

# =========================
# GRAFICO REAL
# =========================

ggplot(df, aes(x=edad)) +
  
  geom_line(aes(y=retiro_programado_real, color="Retiro programado")) +
  
  geom_line(aes(y=renta_vitalicia_real, color="Renta vitalicia")) +
  
  labs(
    title="Pensión real (poder adquisitivo)",
    x="Edad",
    y="Pensión mensual real",
    color="Modalidad"
  ) +
  
  theme_minimal()