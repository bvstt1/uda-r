# Función objetivo
f <- function(x) {
  return((1 - x[1])^2 + 100 * (x[2] - x[1]^2)^2)
}

# Gradiente de la función
grad_f <- function(x) {
  dx <- -2 * (1 - x[1]) - 400 * x[1] * (x[2] - x[1]^2)
  dy <- 200 * (x[2] - x[1]^2)
  return(c(dx, dy))
}

# Parámetros
alpha <- 0.0001              # Puedes experimentar con 0.0001, 0.001, 0.01, etc.
max_iter <- 10000
tol <- 1e-6
x_actual <- c(0, 1)        # Cambia este punto inicial para probar casos distintos

# Para guardar la trayectoria
x_vals <- matrix(nrow = 0, ncol = 2)
f_vals <- numeric()

# Método descenso del gradiente
for (i in 1:max_iter) {
  # Guardar el punto actual y el valor de f(x)
  x_vals <- rbind(x_vals, x_actual)
  f_vals <- c(f_vals, f(x_actual))
  
  grad <- grad_f(x_actual)
  x_nuevo <- x_actual - alpha * grad
  
  if (sqrt(sum(grad^2)) < tol) {
    x_actual <- x_nuevo
    x_vals <- rbind(x_vals, x_actual)
    f_vals <- c(f_vals, f(x_actual))
    break
  }
  
  x_actual <- x_nuevo
}

# Resultado
resultados <- list(minimo = f(x_actual),
                   objetivo = x_actual,
                   iteraciones = i)
print(resultados)

# Crear una malla de valores para las curvas de nivel
x_seq <- seq(-2, 2, length.out = 400)
y_seq <- seq(-1, 3, length.out = 400)
z <- outer(x_seq, y_seq, Vectorize(function(x, y) f(c(x, y))))

# Graficar curvas de nivel

x11()

contour(x_seq, y_seq, z, nlevels = 50,
        xlab = "x", ylab = "y", main = "Descenso del gradiente en la función de Rosenbrock")

# Trayectoria del algoritmo
points(x_vals[,1], x_vals[,2], type = "o", col = "red", pch = 19)

# Mínimo teórico en (1,1)
points(1, 1, col = "blue", pch = 4, lwd = 2)

legend("bottomright", legend = c("Trayectoria", "Mínimo"),
       col = c("red", "blue"), pch = c(19, 4))


