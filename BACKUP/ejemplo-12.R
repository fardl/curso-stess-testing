library(tidyverse)

# =========================
# PARAMETROS
# =========================

edad_inicio  <- 20
edad_retiro  <- 60
edad_muerte  <- 85

salario_inicial <- 30000
crec_salario    <- 0.02
cotizacion      <- 0.084
rendimiento     <- 0.05
tasa_tecnica    <- 0.05

meses_trabajo <- (edad_retiro - edad_inicio)*12

# =========================
# ACUMULACION DEL FONDO
# =========================

edad <- seq(edad_inicio, edad_retiro-1/12, by=1/12)

salario <- salario_inicial*(1+crec_salario)^(edad-edad_inicio)

aporte <- salario*cotizacion

fondo <- numeric(length(edad))

for(i in 2:length(fondo)){
  
  fondo[i] <- fondo[i-1]*(1+rendimiento/12) + aporte[i]
  
}

capital <- tail(fondo,1)

cat("Capital acumulado al retiro:", round(capital),"\n")

# =========================
# SIMULACION DEL RETIRO
# =========================

meses_retiro <- (edad_muerte - edad_retiro)*12

edad_pension <- seq(edad_retiro, edad_muerte-1/12, by=1/12)

pension <- numeric(meses_retiro)
fondo_retiro <- numeric(meses_retiro)

fondo_retiro[1] <- capital

for(t in 1:meses_retiro){
  
  vida_restante <- meses_retiro - t + 1
  
  # factor actuarial restante
  
  anualidad <- sum(1/(1+tasa_tecnica/12)^(1:vida_restante))
  
  pension[t] <- fondo_retiro[t]/anualidad
  
  # evolucion del fondo
  
  if(t < meses_retiro){
    
    fondo_retiro[t+1] <- fondo_retiro[t]*(1+rendimiento/12) - pension[t]
    
  }
  
}

df <- data.frame(
  
  edad = edad_pension,
  pension = pension,
  fondo = fondo_retiro
  
)

# =========================
# GRAFICOS
# =========================

# pension

g1 <- ggplot(df, aes(x=edad, y=pension)) +
  
  geom_line(size=1.2) +
  
  labs(
    title="Evolución de la pensión bajo retiro programado",
    x="Edad",
    y="Pensión mensual"
  ) +
  
  theme_minimal()

# fondo

g2 <- ggplot(df, aes(x=edad, y=fondo)) +
  
  geom_line(size=1.2, color="darkred") +
  
  labs(
    title="Evolución del fondo de pensiones",
    x="Edad",
    y="Capital restante"
  ) +
  
  theme_minimal()

g1
g2