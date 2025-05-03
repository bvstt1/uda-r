# FUNCIÓN OBJETIVO

f <- function(x){
  return(x^2)
}

# FUNCION GRADIENTE

grad_f <- function(x){
  return(2 * x)
}

# PARAMETROS DEL ALGORITMO

alpha <- 0.5
max_iter <- 1000
tol <- 1e-6

# VALOR INICIAL

x_actual <- 10

# METODO DE DESCENSO DE GRADIENTE

for(i in 1:max_iter){
  x_nuevo <- x_actual - alpha * grad_f(x_actual)
  if (abs(x_nuevo - x_actual) < tol){
    break
  }
  x_actual <- x_nuevo
}

# RESULTADOS

resultados <- list(minimo = f(x_nuevo),
                   objetivo = x_nuevo,
                   iteraciones = i)
print(resultados)

