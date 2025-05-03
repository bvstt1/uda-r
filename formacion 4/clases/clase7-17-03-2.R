f <- function(param){
  x <- param[1]
  y <- param[2]
  return(x^2 + y ^ 2)
}

grad_f <- function(param){
  x <- param[1]
  y <- param[2]
  return(c(2*x, 2*y))
}

hess_f <- function(param){
  x <- param[1]
  y <- param[2]
  return(matrix(c(2,0,0,2),
                nrow = 2, ncol = 2,
                byrow = TRUE))
}

# Prametros del algoritmo

max_iter <- 1000
tol <- 1e-6

x_actual <- c(2,2)

# Método de newton - raphson

for (i in 1:max_iter){
  x_nuevo <- x_actual - solve(hess_f(x_actual)) %*% grad_f(x_actual)
  if (norm(grad_f(x_nuevo), "2") < tol){
    break
  }
  x_actual <- x_nuevo
}

# Mostrar los resultados

resultado <- list(minimo = f(x_nuevo),
                  objetivo = as.vector(x_nuevo),
                  iteraciones = i)

