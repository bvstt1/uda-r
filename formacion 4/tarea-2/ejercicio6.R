# Función objetivo
f <- function(x) {
  return((x[1] - 1)^2 + 2 * (x[2] - 2)^2)
}

# Gradiente de la función
grad_f <- function(x) {
  return(c(2 * (x[1] - 1),
           4 * (x[2] - 2)))
}

# Parámetros
alpha <- 0.1
beta <- 0.9
max_iter <- 1000
tol <- 1e-6

### MÉTODO CON MOMENTUM

# Punto inicial y velocidad inicial
x_actual <- c(5, 5)
v_actual <- c(0, 0)

# Guardar trayectoria
x_vals_momentum <- matrix(nrow = 0, ncol = 2)
f_vals_momentum <- numeric()

# Descenso del gradiente con momentum
for (i in 1:max_iter) {
  x_vals_momentum <- rbind(x_vals_momentum, x_actual)
  f_vals_momentum <- c(f_vals_momentum, f(x_actual))
  
  grad <- grad_f(x_actual)
  v_nuevo <- beta * v_actual - alpha * grad
  x_nuevo <- x_actual + v_nuevo
  
  if (sqrt(sum(grad^2)) < tol) {
    x_actual <- x_nuevo
    x_vals_momentum <- rbind(x_vals_momentum, x_actual)
    f_vals_momentum <- c(f_vals_momentum, f(x_actual))
    break
  }
  
  x_actual <- x_nuevo
  v_actual <- v_nuevo
}

resultados_momentum <- list(minimo = f(x_actual),
                            objetivo = x_actual,
                            iteraciones = i)
print(resultados_momentum)

### MÉTODO ESTÁNDAR

# Reiniciar punto inicial
x_actual <- c(5, 5)

# Guardar trayectoria
x_vals_estandar <- matrix(nrow = 0, ncol = 2)
f_vals_estandar <- numeric()

# Descenso del gradiente estándar
for (i in 1:max_iter) {
  x_vals_estandar <- rbind(x_vals_estandar, x_actual)
  f_vals_estandar <- c(f_vals_estandar, f(x_actual))
  
  grad <- grad_f(x_actual)
  x_nuevo <- x_actual - alpha * grad
  
  if (sqrt(sum(grad^2)) < tol) {
    x_actual <- x_nuevo
    x_vals_estandar <- rbind(x_vals_estandar, x_actual)
    f_vals_estandar <- c(f_vals_estandar, f(x_actual))
    break
  }
  
  x_actual <- x_nuevo
}

resultados_estandar <- list(minimo = f(x_actual),
                            objetivo = x_actual,
                            iteraciones = i)
print(resultados_estandar)

### VISUALIZACIÓN COMPARATIVA

# Crear malla para curvas de nivel
x_seq <- seq(-1, 6, length.out = 200)
y_seq <- seq(-1, 6, length.out = 200)
z <- outer(x_seq, y_seq, Vectorize(function(x, y) f(c(x, y))))

# Curvas de nivel
contour(x_seq, y_seq, z, nlevels = 30,
        xlab = "x", ylab = "y", main = "Comparación: Estándar vs Momentum")

# Trayectorias
points(x_vals_estandar[,1], x_vals_estandar[,2], type = "o", col = "blue", pch = 19)
points(x_vals_momentum[,1], x_vals_momentum[,2], type = "o", col = "red", pch = 17)

# Mínimo teórico
points(1, 2, col = "black", pch = 4, lwd = 2)

# Leyenda
legend("topright", legend = c("Estándar", "Momentum", "Mínimo"),
       col = c("blue", "red", "black"), pch = c(19, 17, 4))
