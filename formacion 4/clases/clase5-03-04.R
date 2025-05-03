
f <- function(x){
  return((x[1] - 2)^2 + (x[2] + 3)^2)
}

# GRADIENTE DE LA FUNCION

grad_f <- function(x){
  return(c(2*(x[1]-2),
           2*(x[2]+3)))
}

# PARAMETROS DEL ALGORITMO

alpha <- 0.1
max_iter <- 1000
tol <- 1e-6

# PUNTO IINICIAL

x_actual <- c(-5,5)

# METODO DEL DESCENSO DEL GRADIENTE

for(i in 1:max_iter){
  x_nuevo <- x_actual - alpha * grad_f(x_actual)
  if(norm(grad_f(x_nuevo),"2") < tol){
    break
  }
  x_actual <- x_nuevo
}

resultados <- list(minimo = f(x_nuevo),
                   objetivo = x_nuevo,
                   iteraciones = i)

print(resultados)