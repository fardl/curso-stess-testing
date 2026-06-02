library(tidyverse)

# =========================
# PARAMETROS
# =========================

edad_inicio <- 20
edad_retiro <- 60
edad_muerte_inicial <- 85

salario_inicial <- 30000

crec_real_salario <- 0.01
inflacion <- 0.04

cotizacion <- 0.084

rendimiento_real <- 0.092
tasa_tecnica_real <- 0.092

incremento_longev <- 0.25  # años que aumenta la esperanza de vida cada año

# =========================
# TASAS NOMINALES
# =========================

rendimiento_nominal <- (1+rendimiento_real)*(1+inflacion)-1
tasa_tecnica_nominal <- (1+tasa_tecnica_real)*(1+inflacion)-1

crec_nominal_salario <- (1+crec_real_salario)*(1+inflacion)-1

# =========================
# PERIODOS
# =========================

anios_trabajo <- edad_retiro - edad_inicio
anios_retiro <- edad_muerte_inicial - edad_retiro

# =========================
# ACUMULACION DEL FONDO
# =========================

edad <- seq(edad_inicio, edad_retiro-1)

salario <- salario_inicial*(1+crec_nominal_salario)^(edad-edad_inicio)

aporte <- salario*cotizacion*12  # aportes anuales

fondo <- numeric(length(edad))

for(i in 2:length(fondo)){
  
  fondo[i] <- fondo[i-1]*(1+rendimiento_nominal) + aporte[i]
  
}

capital <- tail(fondo,1)

cat("Capital acumulado al retiro:", round(capital), "\n")

# =========================
# RETIRO PROGRAMADO
# =========================

edad_pension <- seq(edad_retiro, edad_muerte_inicial-1)

pension_rp <- numeric(anios_retiro)
capital_rp <- numeric(anios_retiro)
anualidad <- numeric(anios_retiro)

capital_rp[1] <- capital

vida_restante <- anios_retiro

for(t in 1:anios_retiro){
  
  anualidad[t] <- sum(1/(1+tasa_tecnica_nominal)^(1:round(vida_restante)))
  
  pension_rp[t] <- capital_rp[t]/anualidad[t]
  
  if(t < anios_retiro){
    
    capital_rp[t+1] <- capital_rp[t]*(1+rendimiento_nominal) - pension_rp[t]
    
  }
  
  # actualización actuarial de longevidad
  
  vida_restante <- vida_restante - 1 + incremento_longev
  
}

# =========================
# RENTA VITALICIA
# =========================

anualidad_total <- sum(1/(1+tasa_tecnica_nominal)^(1:anios_retiro))

pension_rv <- rep(capital/anualidad_total, anios_retiro)

capital_rv <- rep(0, anios_retiro)

# =========================
# PENSION REAL
# =========================

inflacion_acum <- (1+inflacion)^(0:(anios_retiro-1))

pension_rp_real <- pension_rp / inflacion_acum
pension_rv_real <- pension_rv / inflacion_acum

# =========================
# DATAFRAME
# =========================

df <- data.frame(
  
  edad = edad_pension,
  
  pension_rp_nominal = pension_rp,
  pension_rp_real = pension_rp_real,
  
  pension_rv_nominal = pension_rv,
  pension_rv_real = pension_rv_real,
  
  capital_rp = capital_rp,
  capital_rv = capital_rv,
  
  anualidad = anualidad
  
)

# =========================
# GRAFICO PENSION NOMINAL
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=pension_rp_nominal,color="Retiro programado"),size=1.2)+
  
  geom_line(aes(y=pension_rv_nominal,color="Renta vitalicia"),size=1.2)+
  
  labs(
    title="Pensión nominal anual",
    x="Edad",
    y="Pesos corrientes",
    color="Modalidad"
  )+
  
  theme_minimal()

# =========================
# GRAFICO PENSION REAL
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=pension_rp_real,color="Retiro programado"),size=1.2)+
  
  geom_line(aes(y=pension_rv_real,color="Renta vitalicia"),size=1.2)+
  
  labs(
    title="Pensión real (poder adquisitivo)",
    x="Edad",
    y="Pesos constantes",
    color="Modalidad"
  )+
  
  theme_minimal()

# =========================
# GRAFICO CAPITAL RESIDUAL
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=capital_rp,color="Retiro programado"),size=1.2)+
  
  geom_line(aes(y=capital_rv,color="Renta vitalicia"),size=1.2)+
  
  labs(
    title="Capital residual",
    x="Edad",
    y="Capital",
    color="Modalidad"
  )+
  
  theme_minimal()

# =========================
# GRAFICO ANUALIDAD
# =========================

ggplot(df,aes(x=edad,y=anualidad))+
  
  geom_line(size=1.2,color="darkblue")+
  
  labs(
    title="Evolución de la anualidad actuarial",
    x="Edad",
    y="Factor de anualidad"
  )+
  
  theme_minimal()