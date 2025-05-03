# EJERCICIO:
# min. f(x,y) = (1.5 - x + xy)^2 + (2.25 - x + xy^2)^2 + (2.625 - x + xy^3)^2

# Grafico

f_graf <- function(x, y){
  return(
    (1.5 - x + (x * y))^2 + 
      (2.25 - x + (x * y^2))^2 + 
      (2.625 - x + (x * y^3))^2
  )
}

x <- -10:10
y <- -10:10
z <- outer(x, y, f_graf)

persp(x, y, z, theta = 25, phi = 35, col = 2)

# Optimizacion

f <- function(x){
  x_1 <- x[1]
  x_2 <- x[2]
  return(
    (1.5 - x_1 + (x_1 * x_2))^2 + 
      (2.25 - x_1 + (x_1 * x_2^2))^2 + 
      (2.625 - x_1 + (x_1 * x_2^3))^2
  )
}

grad_f <- function(x){
  x_1 <- x[1]
  x_2 <- x[2]
  
  d_x <- (
    2 * (1.5 - x_1 + (x_1 * x_2)) * (-1 + x_2) + 
      2 * (2.25 - x_1 + (x_1 * x_2^2)) * (-1 + x_2^2) + 
      2 * (2.625 - x_1 + (x_1 * x_2^3)) * (-1 + (3 * x_2^3))
  )
  
  d_y <- (
    2 * (1.5 - x_1 + (x_1 * x_2)) * x_1 +
      2 * (2.25 - x_1 + (x_1 * x_2)^2) * (2 * x_1 * x_2) +
      2 * (2.625 - x_1 + (x_1 * x_2)^3) * (3 * x_1 * x_2^2)
  )
  
  return(c(d_x, d_y))
}


alpha <- seq(from= 0.05, to=0.01, by = -0.01)

iter_max <- 10000
tol <- 1e-6

resultados <- data.frame(alpha,
                         x_optimo = numeric(length(alpha)),
                         y_optimo = numeric(length(alpha)),
                         iteraciones = numeric(length(alpha)))

# METODO DE DESCENSO DE GRADIENTE

for(k in 1:length(alpha)){
  x_actual <- c(0,0)
  for (i in 1:iter_max){
    x_nuevo <- x_actual - alpha[k] * grad_f(x_actual)
    if (norm(grad_f(x_nuevo), "2") < tol){
      #resultados$x_optimo[k, 2] <- x_nuebo[1]
      #resultados$y_optimo[k,3] <- x_nuevo[2]
      #resultados$iteraciones[k, 4] <- i
      cat("alpha =", alpha[k], "x =", x_nuevo, "iter=", i, "\n")
      break
    }
    x_actual <- x_nuevo
  }
}


print(resultados)
