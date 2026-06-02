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

tasa_tecnica <- 0.03

rendimientos <- c(0.02,0.04,0.06)

meses_trabajo <- (edad_retiro - edad_inicio)*12
meses_retiro  <- (edad_muerte - edad_retiro)*12

# =========================
# FUNCION DE SIMULACION
# =========================

simular_pension <- function(r){
  
  edad <- seq(edad_inicio, edad_retiro-1/12, by=1/12)
  
  salario <- salario_inicial*(1+crec_salario)^(edad-edad_inicio)
  
  aporte <- salario*cotizacion
  
  fondo <- numeric(length(edad))
  
  for(i in 2:length(fondo)){
    
    fondo[i] <- fondo[i-1]*(1+r/12) + aporte[i]
    
  }
  
  capital <- tail(fondo,1)
  
  edad_pension <- seq(edad_retiro, edad_muerte-1/12, by=1/12)
  
  pension <- numeric(meses_retiro)
  fondo_retiro <- numeric(meses_retiro)
  
  fondo_retiro[1] <- capital
  
  for(t in 1:meses_retiro){
    
    vida_restante <- meses_retiro - t + 1
    
    anualidad <- sum(1/(1+tasa_tecnica/12)^(1:vida_restante))
    
    pension[t] <- fondo_retiro[t]/anualidad
    
    if(t < meses_retiro){
      
      fondo_retiro[t+1] <- fondo_retiro[t]*(1+r/12) - pension[t]
      
    }
    
  }
  
  data.frame(
    
    edad = edad_pension,
    pension = pension,
    rendimiento = paste0(r*100,"%")
    
  )
  
}

# =========================
# CORRER SIMULACIONES
# =========================

resultados <- map_df(rendimientos, simular_pension)

# =========================
# GRAFICO
# =========================

ggplot(resultados, aes(x=edad, y=pension, color=rendimiento)) +
  
  geom_line(size=1.2) +
  
  labs(
    title="Trayectoria de pensión bajo retiro programado",
    subtitle="Comparación de distintos rendimientos del fondo",
    x="Edad",
    y="Pensión mensual",
    color="Rendimiento del fondo"
  ) +
  
  theme_minimal()