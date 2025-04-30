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

iter_max <- 10000
alpha <- 0.01
tol <- 1e-6
x_inicial <- 10
y_inicial <- 10
x_actual <- c(x_inicial, y_inicial)

msg <- "Algoritmo no converge"
for(i in iter_max){
  x_siguiente <- x_actual - alpha * grad_f(x_actual) # x_i+1 = x_i - alpha * grad(f(x_i))
  
  if(norm(grad_f(x_siguiente), "2") < tol){
    msg <- "Algoritmo converge"
    break
  }
  
  x_actual <- x_siguiente
}

resultados <- list(
  minimo = f(x_siguiente),
  objetivo = x_siguiente,
  iteraciones = i,
  msg = msg
)

resultados
