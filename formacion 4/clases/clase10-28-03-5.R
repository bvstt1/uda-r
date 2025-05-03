f <- function(x){
  return((x[1] - 1)^2 + (x[2] - 2)^2)
}

grad_f <- function(x){
  return(c(2 * (x[1] - 1), 2 * (x[2] - 2)))
}

x_inicial <- c(0,0)

optim(par = x_inicial,
      fn = f,
      gr = grad_f,
      method = "BFGS")