# Función objetivo
f <- function(x) {
  return(x[1]^2 + 2 * x[2]^2)}

# Gradiente de la función
grad_f <- function(x) {
  return(c(2 * x[1], 4 * x[2]))
}

# Parámetros
alpha0 <- 0.5
tau <- 0.01
max_iter <- 1000
tol <- 1e-6
x_actual <- c(5, 5)

# Para guardar la trayectoria
x_vals <- matrix(nrow = 0, ncol = 2)
f_vals <- numeric()

# Método descenso del gradiente con tasa de aprendizaje variable
for (k in 1:max_iter) {
  
  # Guardar el punto actual y el valor de f(x)
  x_vals <- rbind(x_vals, x_actual)
  f_vals <- c(f_vals, f(x_actual))
  
  # Calcular la tasa de aprendizaje con decaimiento
  alpha_k <- alpha0 / (1 + k * tau)
  
  # Calcular nuevo punto
  grad <- grad_f(x_actual)
  x_nuevo <- x_actual - alpha_k * grad
  
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
                   iteraciones = k)
print(resultados)

# Visualización: curvas de nivel + trayectoria
x_seq <- seq(-6, 6, length.out = 100)
y_seq <- seq(-6, 6, length.out = 100)
z <- outer(x_seq, y_seq, Vectorize(function(x, y) f(c(x, y))))

contour(x_seq, y_seq, z, nlevels = 30,
        xlab = "x", ylab = "y", main = "Descenso con tasa variable (decaimiento)")

points(x_vals[,1], x_vals[,2], col = "red", type = "o", pch = 19)
points(0, 0, col = "blue", pch = 4, lwd = 2)  # mínimo teórico
legend("topleft", legend = c("Trayectoria", "Mínimo"),
       col = c("red", "blue"), pch = c(19, 4))
