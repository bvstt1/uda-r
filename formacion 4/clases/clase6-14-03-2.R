# Definimos la función
f <- function(x) {
  return((x - 2)^2)
}
# Definimos el gradiente de la función
grad_f <- function(x) {
  return(2 * (x - 2))
}
# Parámetros del algoritmo
tol <- 1e-6
max_iter <- 1000
# Vector con valores de alpha
alpha <- c(0.01, 0.10, 0.15, 0.25, 0.50)
# Almacenamos los resultados
resultados <- data.frame(alpha,
                         x_optimo = numeric(length(alpha)),
                         iteracion = numeric(length(alpha)))
# Algoritmo de descenso de gradiente
for (k in 1:length(alpha)) {
  x_actual <- 10
  for(i in 1:max_iter) {x_nuevo <- x_actual - alpha[k] * grad_f(x_actual)
  if (norm(grad_f(x_nuevo), "2") < tol) {
    resultados[k, 2] <- x_nuevo
    resultados[k, 3] <- i
    break
  }
  x_actual <- x_nuevo
  }
}
# Mostramos los resultados
print(resultados)