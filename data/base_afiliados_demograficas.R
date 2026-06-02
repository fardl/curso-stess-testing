# Base de afiliados con aspectos demograficos considerados
rm(list=ls())
# 1. Parametros
set.seed(123)

n <- 10000

# tasas de contribución
tasa_empleado <- 0.0287
tasa_empleador <- 0.0710
tasa_total <- tasa_empleado + tasa_empleador

# edades
edad_min <- 18
edad_max <- 65

# 2. Distribucion de edades
edades <- sample(
  18:65, n, replace = TRUE,
  prob = dnorm(18:65, mean = 35, sd = 10)
)

# 3. Cohortes
cohorte <- cut(edades,
               breaks = c(18,30,50,65),
               labels = c("Joven","Medio","Mayor"),
               include.lowest = TRUE,
               right = TRUE)

sum(is.na(cohorte))

# 4. Matrices de transicion
P_base <- matrix(c(
  0.80, 0.15, 0.05,
  0.30, 0.60, 0.10,
  0.40, 0.30, 0.30
), nrow = 3, byrow = TRUE)

rownames(P_base) <- colnames(P_base) <- c("Formal","Informal","Desempleado")

P_joven <- P_base
P_joven[1,2] <- 0.20
P_joven[1,1] <- 0.75

P_mayor <- P_base
P_mayor[1,1] <- 0.85
P_mayor[1,2] <- 0.10

# 5. Tabla de Mortalidad
edades_tabla <- 18:100
qx <- pmin(0.0005 * exp(0.08*(edades_tabla-30)), 1)

tabla_mortalidad <- data.frame(
  edad = edades_tabla,
  qx = qx
)


# 6. Funciones de simulacion
# 6.1 Vida
simular_vida <- function(edad_inicial){
  edad <- edad_inicial
  
  while(edad < 100){
    qx <- tabla_mortalidad$qx[tabla_mortalidad$edad == edad]
    
    if(runif(1) < qx){
      return(edad)
    }
    
    edad <- edad + 1
  }
  
  return(100)
}

# 6.2 Trayectoria laboral
simular_trayectoria <- function(estado_inicial, P, T){
  estados <- c("Formal","Informal","Desempleado")
  estado <- estado_inicial
  
  trayectoria <- c()
  
  for(t in 1:T){
    trayectoria[t] <- estado
    estado <- sample(estados, 1, prob = P[estado,])
  }
  
  return(trayectoria)
}

#  7.  Simulacion principal
library(dplyr)

resultados <- lapply(1:n, function(i){
  
  edad <- edades[i]
  
  # cohorte define matriz
  if(cohorte[i] == "Joven"){
    P <- P_joven
  } else if(cohorte[i] == "Mayor"){
    P <- P_mayor
  } else {
    P <- P_base
  }
  
  # estado inicial
  estado_inicial <- sample(c("Formal","Informal","Desempleado"), 1,
                           prob = c(0.5,0.4,0.1))
  
  # vida
  edad_muerte <- simular_vida(edad)
  
  # horizonte laboral
  T <- edad_muerte - edad
  
  trayectoria <- simular_trayectoria(estado_inicial, P, T)
  
  # conteos
  meses_formal <- sum(trayectoria == "Formal") * 12
  meses_informal <- sum(trayectoria == "Informal") * 12
  meses_desempleo <- sum(trayectoria == "Desempleado") * 12
  
  meses_laborales <- meses_formal + meses_informal + meses_desempleo
  
  # salario
  salario <- rlnorm(1, log(25000), 0.6)
  
  # cotizaciones (solo formal)
  meses_cotizados <- meses_formal
  
  densidad <- ifelse(meses_laborales > 0,
                     meses_cotizados / meses_laborales, 0)
  
  # aportes
  aporte_mensual <- salario * tasa_total
  aporte_total <- aporte_mensual * meses_cotizados
  
  # capitalización
  saldo <- aporte_total * (1.05)^(T)
  
  return(data.frame(
    id = i,
    edad = edad,
    cohorte = cohorte[i],
    edad_muerte = edad_muerte,
    salario = salario,
    meses_formal = meses_formal,
    meses_informal = meses_informal,
    meses_desempleo = meses_desempleo,
    meses_cotizados = meses_cotizados,
    meses_laborales = meses_laborales,
    densidad = densidad,
    saldo = saldo
  ))
})

# 8. Base final
afiliados <- bind_rows(resultados)

