f <- function(param){
  x <- param[1]
  y <- param[2]
  return((1 - x)^2 + 100 * (y - x^2)^2)
}

grad_f <- function(param){
  x <- param[1]
  y <- param[2]
  d_x <- -2*(1-x) - 400 * (y - x^2)
  d_y <- 200 * (y - x^2)
  return(c(d_x, d_y))
}

hess_f <- function(param){
  x <- param[1]
  y <- param[2]
  d_xx <- 2 - 400 * (y - 3 * x ^ 2)
  d_yy <- 200
  d_xy <- d_yx <- - 400 * x
  return(matrix(c(d_xx, d_xy,
                  d_yx, d_yy),
                nrow = 2, ncol = 2,
                byrow = TRUE))
}

# Prametros

max_iter <- 1000
tol <- 1e-6

# Punto Inicial

x_actual <- c(-2,1)

# Método de Newton - Raphson

for (i in 1:max_iter){
  hessian <- hess_f(x_actual)
  if(any(eigen(hessian)$values)) <= 0{
    hessian <- hessian + diag(abs(min(eigen(hessian)$values)),2)
  }
  x_nuevo <- x_actual - solve(hess_f(x_actual)) %*% grad_f(x_actual)
  if(norm(grad_f(x_actual), "2") < tol){
    break
  }
  x_actual <- x_nuevo
}

# Mostrar los resultados

resultado <- list(minimo = f(x_nuevo),
                  objetivo = as.vector(x_nuevo),
                  iteraciones = i)

print(resultado)
