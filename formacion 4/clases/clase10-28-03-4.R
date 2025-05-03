rosenbrock <- function(x){
  return(100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2)
}


x_inicial <- c(-1.2,1)

optim(par = x_inicial,
      fn = rosenbrock,
      method = "BFGS") #MALO

