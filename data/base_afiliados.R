# Creacion de base de datos de cotizantes para fines didacticos
rm(list=ls())
set.seed(123)

n <- 10000

# Parámetros sistema dominicano (aproximación)
tasa_empleado <- 0.0287
tasa_empleador <- 0.0710
tasa_total <- tasa_empleado + tasa_empleador

edad_min <- 18
edad_max <- 65

# Distribucion de las edades
edades <- sample(
  18:65, n, replace = TRUE,
  prob = dnorm(18:65, mean = 35, sd = 10)
)

# Vida laboral
anios_laborales <- pmax(edades - 18, 0)
meses_laborales <- anios_laborales * 12

# Informalidad
prop_informal <- runif(n, 0.3, 0.7)
prop_desempleo <- runif(n, 0.05, 0.2)

meses_informal <- round(meses_laborales * prop_informal)
meses_desempleo <- round(meses_laborales * prop_desempleo)

meses_cotizados <- meses_laborales - meses_informal - meses_desempleo
meses_cotizados <- pmax(meses_cotizados, 0)

# Densidad de cotizacion
densidad <- meses_cotizados / meses_laborales
densidad[is.na(densidad)] <- 0


# Salarios
salario <- rlnorm(n, log(25000), 0.6)

# Historia contribuciones
aporte_mensual <- salario * tasa_total

aporte_acumulado <- meses_cotizados * aporte_mensual

#Rentabilidad y saldo
rendimiento_prom <- 0.05

saldo <- aporte_acumulado * (1 + rendimiento_prom)^(anios_laborales)


# Base final
afiliados <- data.frame(
  id = 1:n,
  edad = edades,
  salario = salario,
  meses_laborales = meses_laborales,
  meses_cotizados = meses_cotizados,
  meses_informal = meses_informal,
  meses_desempleo = meses_desempleo,
  densidad = densidad,
  tasa_empleado = tasa_empleado,
  tasa_empleador = tasa_empleador,
  saldo = saldo
)





P <- matrix(c(
  0.80, 0.15, 0.05,   # Formal
  0.30, 0.60, 0.10,   # Informal
  0.40, 0.30, 0.30    # Desempleado
), nrow = 3, byrow = TRUE)

colnames(P) <- rownames(P) <- c("Formal", "Informal", "Desempleado")


simular_estado <- function(estado_inicial, P, T){
  estados <- c("Formal", "Informal", "Desempleado")
  estado <- estado_inicial
  trayectoria <- c(estado)
  
  for(t in 1:T){
    probs <- P[estado, ]
    estado <- sample(estados, 1, prob = probs)
    trayectoria <- c(trayectoria, estado)
  }
  
  return(trayectoria)
}


afiliados$cohorte <- cut(afiliados$edad,
                         breaks = c(18,30,50,65),
                         labels = c("Joven","Medio","Mayor"))


P_joven <- P
P_joven[1,2] <- 0.20  # más informalidad

P_mayor <- P
P_mayor[1,1] <- 0.85  # más estabilidad formal


edades <- 18:100

qx <- pmin(0.0005 * exp(0.08*(edades-30)), 1)

tabla_mortalidad <- data.frame(
  edad = edades,
  qx = qx,
  px = 1 - qx
)

simular_vida <- function(edad_inicial, tabla){
  edad <- edad_inicial
  
  while(edad < 100){
    qx <- tabla$qx[tabla$edad == edad]
    
    if(runif(1) < qx){
      return(edad)
    }
    
    edad <- edad + 1
  }
  
  return(100)
}


simular_individuo <- function(edad, estado_inicial, P, tabla_mort){
  
  edad_muerte <- simular_vida(edad, tabla_mort)
  T <- edad_muerte - edad
  
  trayectoria <- simular_estado(estado_inicial, P, T)
  
  return(list(
    edad_muerte = edad_muerte,
    trayectoria = trayectoria
  ))
}
simular_individuo(42, "Formal", P, tabla_mortalidad)
