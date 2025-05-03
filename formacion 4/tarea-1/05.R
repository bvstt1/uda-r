# EJERCICIO 5 - ENCONTRAR EL MINIMO DE LA FUNCION - BUSQUEDA ALEATORIA

#FUNCION

f <- function(x){
  return(x[1]^2 + x[2]^2 + x[3]^2 + cos(x[1] * x[2]* x[3]))
}

# PARÁMETROS
set.seed(14)
n <- 100

# VECTORIZACIÓN Y ANÁLISIS - DOMINIO CONTINUO
x <- runif(n, min = -2, max = 2)
y <- runif(n, min = -2, max = 2)
z <- runif(n, min = -2, max = 2)

dominio <- cbind(x, y, z)

valores <- apply(dominio, 1, f)

index_min <- which.min(valores)
punto_min <- dominio[index_min, ]
f_min <- valores[index_min]

cat("El valor mínimo de f(x, y, z) es:", f_min,
    "\nEn el punto (x, y, z):", punto_min)