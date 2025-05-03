# DEFINIMOS FUNCION OBJETIVO

f <- function(x){
  return((x - 2)^2)
}

# DEFINIMOS FUNCION GRADIENTE DE LA FUNCION

grad_f <- function(x){
  return(2 * (x-2))
}

# PARAMETROS DEL ALGORITMO

alpha <- 0.1
max_iter <- 1000
tol <- 1e-6

# METODO DE DESCENSO DE GRADIENTE

for (i in 1:max_iter){
  x_nuevo <- x_actual - alpha * grad_f(x_actual)
  if(abs(grad_f(x_nuevo)) < tol){
    break
  }
  x_actual <- x_nuevo
}

# MOSTRAMOS RESULTADOS

resultados <- list(minimo = f(x_nuevo),
                   objetivo = x_nuevo,
                   iteraciones = i)

print(resultados) #malo
 