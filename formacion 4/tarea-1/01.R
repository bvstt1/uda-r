# EJERCICIO 1 - ENCONTRAR EL MINIMO DE LA FUNCION - FUERZA BRUTA

# FUNCION
f <- function(x){
  return ((x - 3)^2 + 5)
} 

# VECTORIZACIÓN Y ANÁLISIS - DOMINIO DISCRETO
x <- -5:10
y <- f(x)
cbind(x,y)

index_min <- which.min(y)
x_min <- x[index_min]
y_min <- y[index_min]

# GRAFICO
par(mar = c(4,4,1,1))
plot(x,y, pch = 18)
points(x_min, y_min, pch = 19, col = "red")

# MENSAJE
cat("El valor minimo de la función es:", y_min,
    "\nEl valor optimo de x es:", x_min)
