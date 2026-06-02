library(rugarch)
spec <- ugarchspec(
  variance.model = list(model = "sGARCH",
                        garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "std"
)
fit_garch <- ugarchfit(spec = spec, data = datos$ret_port)
show(fit_garch)

plot(fit_garch)

sigma_t <- sigma(fit_garch)
mu_t <- fitted(fit_garch)
alpha <- 0.99
q_t <- qt(alpha, df = 6) # aproximación t-Student
VaR_99 <- -(mu_t + sigma_t * q_t)
summary(VaR_99)
