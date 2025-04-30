#cargar libreria
library(optimx)

# Función opbjetivo
f <- function(x){
  return((x[1] - 2)^2 + (x[2] + 3)^2)
}

# Vector inicial
x_inicial <- c(1, -1)

# Función optimx
optimx(par = x_inicial,
       fn = f,
       method = "BFGS")

# Límites para las variables

lim_inf <- c(1, -5)
lim_sup <- c(3, -1)

optimx(par = x_inicial,
       fn = f,
       method = "L-BFGS-B",
       lower = lim_inf,
       upper = lim_sup)
