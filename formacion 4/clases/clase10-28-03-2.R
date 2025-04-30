# Función objetivo (min)

f <- function(x, a, b){
  return((x-a)^2 + b)
}

optimize(f,
         interval = c(-1,5),
         a = 2, b = 1)

