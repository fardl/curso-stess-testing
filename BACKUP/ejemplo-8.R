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

# conversion a tasas nominales

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

cat("Capital nominal al retiro:", round(capital),"\n")

# =========================
# RETIRO PROGRAMADO
# =========================

edad_pension <- seq(edad_retiro, edad_muerte-1/12, by=1/12)

pension <- numeric(meses_retiro)
fondo_retiro <- numeric(meses_retiro)

fondo_retiro[1] <- capital

for(t in 1:meses_retiro){
  
  if(t==1 | (t-1)%%12==0){
    
    vida_restante <- meses_retiro - t + 1
    
    anualidad <- sum(1/(1+tasa_tecnica_nominal/12)^(1:vida_restante))
    
    pension_base <- fondo_retiro[t]/anualidad
    
  }
  
  if(t==1){
    
    pension[t] <- pension_base
    
  } else {
    
    pension[t] <- pension[t-1]*(1+inflacion/12)
    
  }
  
  if(t < meses_retiro){
    
    fondo_retiro[t+1] <- fondo_retiro[t]*(1+rendimiento_nominal/12) - pension[t]
    
  }
  
}

# =========================
# CALCULO PENSION REAL
# =========================

inflacion_acum <- (1+inflacion/12)^(0:(meses_retiro-1))

pension_real <- pension / inflacion_acum

# =========================
# DATAFRAME FINAL
# =========================

df <- data.frame(
  
  edad = edad_pension,
  pension_nominal = pension,
  pension_real = pension_real,
  fondo = fondo_retiro
  
)

# =========================
# GRAFICO PENSION
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=pension_nominal,color="Nominal"),size=1.2)+
  
  geom_line(aes(y=pension_real,color="Real"),size=1.2)+
  
  labs(
    title="Trayectoria de la pensión",
    x="Edad",
    y="Pensión mensual",
    color="Tipo"
  )+
  
  theme_minimal()

# =========================
# GRAFICO CAPITAL
# =========================

ggplot(df,aes(x=edad,y=fondo))+
  
  geom_line(size=1.2,color="darkred")+
  
  labs(
    title="Capital restante del fondo",
    x="Edad",
    y="Capital nominal"
  )+
  
  theme_minimal()