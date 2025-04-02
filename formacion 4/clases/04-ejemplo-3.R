# FUNCION OBJETIVO (min)

# x = [x1 = x, x2 = y]

f <- function(x){
  return(x[1]^2 + 2* x[2]^2 + 10)
}

#f <- function(x, y){
#  return(x^2 + 2* y^2 + 10)
#}

#x <- -10:10
#y <- -10:10
#z <- outer(x,y, f)

#persp(x,y,z, theta = 45, phi = -45, col = "green")


step <- 1
x <- seq(from = -10, to = 10, by = step)
x <- seq(from = -10, to = 10, by = step)
dominio <- expand.grid(x, y)

z <- f(dominio)

resultado <- as.matrix(cbind(dominio, z))

index_min <- which.min(resultado[, 3])

punto_min <- resultado[index_min, 1:2]
f_min <- resultado[index_min, 3]

cat("El valor minimo de f(x,y) es:", f_min,
    "\nEl valor optimo de (x,y) es:", punto_min)

