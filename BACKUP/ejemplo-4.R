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

rendimiento_real <- 0.04
tasa_tecnica_real <- 0.03

incremento_longev <- 0.2   # meses que aumenta la esperanza de vida cada mes

# =========================
# TASAS NOMINALES
# =========================

rendimiento_nominal <- (1+rendimiento_real)*(1+inflacion)-1
tasa_tecnica_nominal <- (1+tasa_tecnica_real)*(1+inflacion)-1

r_m <- (1+rendimiento_nominal)^(1/12)-1
i_m <- (1+tasa_tecnica_nominal)^(1/12)-1

inflacion_m <- (1+inflacion)^(1/12)-1

# =========================
# PERIODOS
# =========================

meses_trabajo <- (edad_retiro-edad_inicio)*12
meses_retiro <- (edad_muerte_inicial-edad_retiro)*12

# =========================
# ACUMULACION DEL FONDO
# =========================

edad <- seq(edad_inicio, edad_retiro-1/12, by=1/12)

crec_nominal_salario <- (1+crec_real_salario)*(1+inflacion)-1

salario <- salario_inicial*(1+crec_nominal_salario)^(edad-edad_inicio)

aporte <- salario*cotizacion

fondo <- numeric(length(edad))

for(i in 2:length(fondo)){
  
  fondo[i] <- fondo[i-1]*(1+r_m) + aporte[i]
  
}

capital <- tail(fondo,1)

cat("Capital acumulado al retiro:", round(capital), "\n")

# =========================
# RETIRO PROGRAMADO
# =========================

edad_pension <- seq(edad_retiro, edad_muerte_inicial-1/12, by=1/12)

pension_rp <- numeric(meses_retiro)
fondo_rp <- numeric(meses_retiro)
anualidad <- numeric(meses_retiro)

vida_restante <- meses_retiro

fondo_rp[1] <- capital

for(t in 1:meses_retiro){
  
  anualidad[t] <- sum(1/(1+i_m)^(1:round(vida_restante)))
  
  pension_rp[t] <- fondo_rp[t]/anualidad[t]
  
  if(t < meses_retiro){
    
    fondo_rp[t+1] <- fondo_rp[t]*(1+r_m) - pension_rp[t]
    
  }
  
  # actualización actuarial de longevidad
  
  vida_restante <- vida_restante - 1 + incremento_longev
  
}

# =========================
# RENTA VITALICIA
# =========================

anualidad_total <- sum(1/(1+i_m)^(1:meses_retiro))

pension_rv <- rep(capital/anualidad_total, meses_retiro)

fondo_rv <- rep(0, meses_retiro)

# =========================
# PENSION REAL
# =========================

inflacion_acum <- (1+inflacion_m)^(0:(meses_retiro-1))

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
  
  capital_rp = fondo_rp,
  capital_rv = fondo_rv,
  
  anualidad = anualidad
  
)

# =========================
# GRAFICO PENSION NOMINAL
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=pension_rp_nominal,color="Retiro programado"),size=1)+
  
  geom_line(aes(y=pension_rv_nominal,color="Renta vitalicia"),size=1)+
  
  labs(
    title="Pensión nominal",
    x="Edad",
    y="Pesos corrientes",
    color="Modalidad"
  )+
  
  theme_minimal()

# =========================
# GRAFICO PENSION REAL
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=pension_rp_real,color="Retiro programado"),size=1)+
  
  geom_line(aes(y=pension_rv_real,color="Renta vitalicia"),size=1)+
  
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
  
  geom_line(aes(y=capital_rp,color="Retiro programado"),size=1)+
  
  geom_line(aes(y=capital_rv,color="Renta vitalicia"),size=1)+
  
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