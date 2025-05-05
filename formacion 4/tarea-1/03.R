# EJERCICIO 3 - ENCONTRAR EL MINIMO DE LA FUNCION - BUSQUEDA ALEATORIA

# FUNCIÓN
f <- function(x){
  (x[1] - 2)^2 + (x[2] + 3)^2
}

# PARÁMETROS
set.seed(14)
n <- 10000

# VECTORIZACIÓN Y ANÁLISIS - DOMINIO CONTINUO
x <- runif(n, min = -10, max = 10)
y <- runif(n, min = -10, max = 10)


dominio <- cbind(x, y) 
z <- apply(dominio, 1, f)

resultado <- as.matrix(cbind(dominio, z))
index_min <- which.min(resultado[, 3])

punto_min <- resultado[index_min, 1:2]
f_min <- resultado[index_min, 3]

# MENSAJE
cat("El valor minimo de f(x,y) es:", f_min,
    "\nEl valor optimo de (x,y) es:", punto_min)
