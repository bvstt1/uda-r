# Función objetivo
f <- function(x){
  return(x^2 + 4*x + 5)
}

# Gradiente de la función
grad_f <- function(x){
  return(2*x + 4)
}

# Parámetros
alpha <- 0.1
max_iter <- 1000
tol <- 1e-6
x_actual <- -10

# Para guardar la trayectoria
x_vals <- numeric()
f_vals <- numeric()

# Método descenso del gradiente
for(i in 1:max_iter){
  
  # Guardar el punto actual y el valor de f(x)
  x_vals <- c(x_vals, x_actual)
  f_vals <- c(f_vals, f(x_actual))
  
  # Calcular nuevo punto
  x_nuevo <- x_actual - alpha * grad_f(x_actual)
  
  if (abs(grad_f(x_nuevo)) < tol){
    break
  }
  x_actual <- x_nuevo
}

# Resultado
resultados <- list(minimo = f(x_nuevo),
                   objetivo = x_nuevo,
                   iteraciones = i)
print(resultados)

# Gráfico de la función
curve(f, from = -12, to = 2, col = "blue", lwd = 2,
      ylab = "f(x)", xlab = "x", main = "Descenso del Gradiente")

# Trayectoria del algoritmo
points(x_vals, f_vals, col = "red", pch = 19, type = "b")
legend("topright", legend = c("Función", "Trayectoria del algoritmo"),
       col = c("blue", "red"), lty = c(1, NA), pch = c(NA, 19))


