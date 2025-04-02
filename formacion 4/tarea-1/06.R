# EJERCICIO 6 - ENCONTRAR EL MINIMO DE LA FUNCION - BUSQUEDA ALEATORIA

# FUNCION
f <- function(x) {
  return(sum((x - 1:4)^2))
}

x <- 0:6

dominio <- expand.grid(x, x, x, x)

valores_funcion <- apply(dominio, 1, f)

# ENCONTRAR EL MÍNIMO
index_min <- which.min(valores_funcion)
punto_min <- dominio[index_min, ]
f_min <- valores_funcion[index_min]

cat("El valor mínimo de f(x1, x2, x3, x4) es:", f_min, 
    "\nEl punto óptimo (x1, x2, x3, x4) es:", as.numeric(punto_min))