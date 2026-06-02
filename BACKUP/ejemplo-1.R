library(tidyverse)

# =========================
# PARAMETROS
# =========================

edad_inicio <- 20
edad_retiro <- 60
edad_max <- 110

salario_inicial <- 30000

crec_real_salario <- 0.01
cotizacion <- 0.084

rendimiento_real <- 0.03
tasa_tecnica_real <- 0.03

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

anualidad <- function(x,r){
  
  edades_restantes <- (edad_max-x)
  
  sum(
    sapply(1:edades_restantes,
           function(t) prob_supervivencia(x,t)/(1+r)^t)
  )
  
}

# =========================
# ACUMULACION DEL FONDO
# =========================

edad_trabajo <- seq(edad_inicio,edad_retiro-1)

salario <- salario_inicial*(1+crec_real_salario)^(edad_trabajo-edad_inicio)

aporte <- salario*cotizacion*12

fondo <- numeric(length(edad_trabajo))

for(i in 2:length(fondo)){
  
  fondo[i] <- fondo[i-1]*(1+rendimiento_real) + aporte[i]
  
}

capital <- tail(fondo,1)

cat("Capital real acumulado:",round(capital),"\n")

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
  
  annuity_vec[t] <- anualidad(edad_actual,tasa_tecnica_real)
  
  pension_rp[t] <- capital_rp[t]/annuity_vec[t]
  
  if(t < length(edad_pension)){
    
    capital_rp[t+1] <- capital_rp[t]*(1+rendimiento_real) - pension_rp[t]
    
  }
}

# =========================
# RENTA VITALICIA
# =========================

annuity_initial <- anualidad(edad_retiro,tasa_tecnica_real)

pension_rv <- rep(capital/annuity_initial,length(edad_pension))

capital_rv <- rep(0,length(edad_pension))

# =========================
# DATAFRAME
# =========================

df <- data.frame(
  
  edad = edad_pension,
  
  pension_rp = pension_rp,
  pension_rv = pension_rv,
  
  capital_rp = capital_rp,
  capital_rv = capital_rv,
  
  anualidad = annuity_vec
  
)

# =========================
# GRAFICO PENSION
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=pension_rp,color="Retiro programado"),size=1.2)+
  
  geom_line(aes(y=pension_rv,color="Renta vitalicia"),size=1.2)+
  
  labs(
    title="Pensión real",
    x="Edad",
    y="Pesos reales",
    color="Modalidad"
  )+
  
  theme_minimal()

# =========================
# GRAFICO CAPITAL
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=capital_rp,color="Retiro programado"),size=1.2)+
  
  geom_line(aes(y=capital_rv,color="Renta vitalicia"),size=1.2)+
  
  labs(
    title="Capital residual real",
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
    title="Anualidad actuarial real",
    x="Edad",
    y="Factor de anualidad"
  )+
  
  theme_minimal()

# =========================
# PROBABILIDAD DE SUPERVIVENCIA
# =========================

prob_supervivencia_cond <- function(x,t){
  
  prod(tabla_mortalidad$px[(x+1):(x+t)])
  
}

# vector de probabilidades de supervivencia

px_t <- sapply(0:(length(edad_pension)-1),
               function(t) prob_supervivencia_cond(edad_retiro,t))

# =========================
# CONSUMO ESPERADO
# =========================

consumo_rp_esperado <- pension_rp * px_t
consumo_rv_esperado <- pension_rv * px_t

df$consumo_rp_esperado <- consumo_rp_esperado
df$consumo_rv_esperado <- consumo_rv_esperado
df$prob_supervivencia <- px_t

# =========================
# GRAFICO
# =========================

ggplot(df,aes(x=edad))+
  
  geom_line(aes(y=consumo_rp_esperado,
                color="Retiro programado"),size=1.2)+
  
  geom_line(aes(y=consumo_rv_esperado,
                color="Renta vitalicia"),size=1.2)+
  
  labs(
    title="Consumo esperado ponderado por probabilidad de supervivencia",
    x="Edad",
    y="Consumo esperado",
    color="Modalidad"
  )+
  
  theme_minimal()