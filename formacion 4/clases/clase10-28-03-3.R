# Función objetivo

f <- function(x){
  return((x - 3)^2)
}

optim( par = 0, fn = f)

