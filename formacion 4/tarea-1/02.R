# EJERCICIO 2 - ENCONTRAR EL MAXIMO DE LA FUNCIÓN - FUERZA BRUTA

# FUNCIÓN
f <- function(x){
  return(-x^3 + 4 * x^2 + 5)
}

# VECTORIZACIÓN Y ANÁLISIS - DOMINIO CONTINUO
step <- 0.1
x <- seq(from = 0, to = 5, by = step)
y <- f(x)
cbind(x,y)

index_max <- which.max(y)
x_max <- x[index_max]
y_max <- y[index_max]

# GRAFICO
par(mar = c(4,4,1,1))
plot(x,y, pch = 18)
points(x_max, y_max, pch = 19, col = "red")

# MENSAJE
cat("El valor maximo de la función es:", y_max,
    "\nEl valor optimo de x es:", x_max)
