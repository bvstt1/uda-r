# Funcción Objetivo
f <- function(x){
  return(x^2)
}

# Gradiente de la función

grad_f <- function(x){
  return(2 * x)
}

# Hessiano de la función
hess_f <- function(x){
  return(2)
}

# Prametros del algoritmo

max_iter <- 1000
tol <- 1e-6

# Valor Inicial

x_actual <- 3

# Metodo de newton - raphson

for (i in 1:max_iter){
  x_nuevo <- x_actual - grad_f(x_actual) / hess_f(x_actual)
  if (norm(grad_f(x_nuevo),"2") < tol){
    break
  }
  x_actual <- x_nuevo
}

# Mostrar los resultados

resultado <- list(minimo = f(x_nuevo),
                  objetivo = x_nuevo,
                  iteraciones = i)