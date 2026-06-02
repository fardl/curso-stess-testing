library(tidyverse)

# =========================
# PARAMETROS
# =========================

edad_inicio <- 20
edad_retiro <- 60
edad_max <- 110

salario_inicial <- 30000

crec_real_salario <- 0.01
inflacion <- 0.04

cotizacion <- 0.084

rendimiento_real <- 0.04
tasa_tecnica_real <- 0.03

# tasas nominales

rendimiento_nominal <- (1+rendimiento_real)*(1+inflacion)-1
tasa_tecnica_nominal <- (1+tasa_tecnica_real)*(1+inflacion)-1

# =========================
# TABLA DE MORTALIDAD
# =========================

edad <- seq(0,edad_max)

qx <- pmin(0.0005*exp(0.08*(edad-30)),0.35)

px <- 1-qx

tabla_mortalidad <- data.frame(
  edad = edad,
  qx = qx,
  px = px
)

# =========================
# FUNCION SUPERVIVENCIA
# =========================

prob_supervivencia <- function(x,t){
  
  prod(tabla_mortalidad$px[(x+1):(x+t)])
  
}

# =========================
# ANUALIDAD ACTUARIAL
# =========================

anualidad <- function(x,i){
  
  edades_restantes <- (edad_max-x)
  
  sum(
    sapply(1:edades_restantes,
           function(t) prob_supervivencia(x,t)/(1+i)^t)
  )
  
}

# =========================
# ACUMULACION DEL FONDO
# =========================

edad_trabajo <- seq(edad_inicio,edad_retiro-1)

crec_nominal_salario <- (1+crec_real_salario)*(1+inflacion)-1

salario <- salario_inicial*(1+crec_nominal_salario)^(edad_trabajo-edad_inicio)

aporte <- salario*cotizacion*12

fondo <- numeric(length(edad_trabajo))

for(i in 2:length(fondo)){
  
  fondo[i] <- fondo[i-1]*(1+rendimiento_nominal) + aporte[i]
  
}

capital <- tail(fondo,1)

cat("Capital acumulado:",round(capital),"\n")

# =========================
# RETIRO PROGRAMADO
# =========================

edad_pension <- seq(edad_retiro,edad_max)

capital_rp <- numeric(length(edad_pension))
pension_rp <- numeric(length(edad_pension))
annuity_vec <- numeric(length(edad_pension))

capital_rp[1] <- capital

for(t in 1:length(edad_pension)){
  
  edad_actual <- edad_pension[t]
  
  annuity_vec[t] <- anualidad(edad_actual,tasa_tecnica_nominal)
  
  pension_rp[t] <- capital_rp[t]/annuity_vec[t]
  
  if(t < length(edad_pension)){
    
    capital_rp[t+1] <- capital_rp[t]*(1+rendimiento_nominal) - pension_rp[t]
    
  }
}

# =========================
# RENTA VITALICIA
# =========================

annuity_initial <- anualidad(edad_retiro,tasa_tecnica_nominal)

pension_rv <- rep(capital/annuity_initial,length(edad_pension))

capital_rv <- rep(0,length(edad_pension))

# =========================
# PENSION REAL
# =========================

inflacion_acum <- (1+inflacion)^(0:(length(edad_pension)-1))

pension_rp_real <- pension_rp/inflacion_acum
pension_rv_real <- pension_rv/inflacion_acum

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
  
  anualidad = annuity_vec
  
)

# =========================
# GRAFICOS
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=pension_rp_nominal,color="Retiro programado"))+
  
  geom_line(aes(y=pension_rv_nominal,color="Renta vitalicia"))+
  
  theme_minimal()+
  
  labs(title="Pensión nominal")

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=pension_rp_real,color="Retiro programado"))+
  
  geom_line(aes(y=pension_rv_real,color="Renta vitalicia"))+
  
  theme_minimal()+
  
  labs(title="Pensión real")

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=capital_rp,color="Retiro programado"))+
  
  geom_line(aes(y=capital_rv,color="Renta vitalicia"))+
  
  theme_minimal()+
  
  labs(title="Capital residual")

ggplot(df,aes(x=edad,y=anualidad))+
  
  geom_line(color="darkblue",size=1)+
  
  theme_minimal()+
  
  labs(title="Anualidad actuarial con probabilidades de supervivencia")