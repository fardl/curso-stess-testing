# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Ejercicios Modulo 5 - Simulacion de Ahorro para el Ret
# Parametros de base
edad_inicio  <- 25
edad_retiro  <- 65
anios        <- edad_retiro - edad_inicio

salario0     <- 30000
g_salario    <- 0.015
tasa_aporte  <- 0.12
retorno      <- 0.05
densidad     <- 0.80

tasa_tecnica <- 0.03
vida_retiro  <- 20

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Funcion de acumulacion previsional
acumular_fondo <- function(salario0, g, aporte, densidad,
                           retorno, anios) {
  fondo <- 0
  salario <- salario0

  for (t in 1:anios) {
    contribucion <- salario * aporte * densidad
    fondo <- (fondo + contribucion) * (1 + retorno)
    salario <- salario * (1 + g)
  }

    return(list(fondo = fondo, salario_final = salario))
                           
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # Factor de anualidad simple
factor_anualidad <- function(i, n) {
  sum(1 / (1 + i)^(1:n))
}

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculo de pension y tasa de reemplazo

res <- acumular_fondo(salario0, g_salario, tasa_aporte,
                      densidad, retorno, anios)

ax <- factor_anualidad(tasa_tecnica, vida_retiro)
pension_anual <- res$fondo / ax
tr <- pension_anual / (12 * res$salario_final)
tr

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
escenarios <- data.frame(
  nombre = c("Baseline", "Adverso", "Severo"),
  retorno = c(0.05, 0.02, 0.00),
  densidad = c(0.80, 0.65, 0.50),
  vida_retiro = c(20, 25, 28)
)

calcular_escenario <- function(r, d, n) {
  res <- acumular_fondo(salario0, g_salario, tasa_aporte,
                        d, r, anios)
  pension <- res$fondo / factor_anualidad(tasa_tecnica, n)
  tr <- pension / (12 * res$salario_final)
  c(fondo = res$fondo, pension_anual = pension, TR = tr)
}

resultados <- t(mapply(calcular_escenario,
                       escenarios$retorno,
                       escenarios$densidad,
                       escenarios$vida_retiro))
cbind(escenarios, round(resultados, 3))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Montecarlo actuarial

set.seed(123)
N <- 10000

retornos  <- rnorm(N, mean = 0.04, sd = 0.08)
densidades <- pmin(pmax(rnorm(N, mean = 0.75, sd = 0.12), 0.30), 1)
vida_ret  <- round(pmax(rnorm(N, mean = 22, sd = 3), 15))

simular_TR <- function(r, d, n) {
  res <- acumular_fondo(salario0, g_salario, tasa_aporte,
                        d, r, anios)
  pension <- res$fondo / factor_anualidad(tasa_tecnica, n)
  pension / (12 * res$salario_final)
}

TR_sim <- mapply(simular_TR, retornos, densidades, vida_ret)

summary(TR_sim)
quantile(TR_sim, probs = c(0.05, 0.50, 0.95))
mean(TR_sim < 0.50)