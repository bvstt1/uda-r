# EJERCICIO 4 - ENCONTRAR EL MAXIMO DE LA FUNCION - FUERZA BRUTA

#FUNCIÓN
f <- function(x){
  return(sin(x[1]) + cos(x[2]) + 0.1 * x[1] * x[2])
}

step <- 0.1
x <- seq(from = -pi, to = pi, by = step)
y <- seq(from = -pi, to = pi, by = step)

dominio <- expand.grid(x, y)

z <- apply(dominio, 1, f) # cos sin tan

resultado <- as.matrix(cbind(dominio, z))
index_max <- which.max(resultado[, 3])

punto_max <- resultado[index_max, 1:2]
f_max <- resultado[index_max, 3]

# MOSTRAR RESULTADO
cat("El valor máximo de f(x, y) es:", f_max,
    "\nEl punto óptimo (x, y) es:", punto_max)