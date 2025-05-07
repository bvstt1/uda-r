# Función objetivo
f <- function(xy) {
  (xy[1] - 2)^2 + (xy[2] + 1)^2
}

# Gradiente de la función
grad_f <- function(xy) {
  c(2 * (xy[1] - 2),
    2 * (xy[2] + 1))
}

# Parámetros
alpha <- 0.1
max_iter <- 1000
tol <- 1e-6
xy_actual <- c(5, -5)

# Guardar trayectoria
trayectoria <- matrix(nrow = 0, ncol = 2)

for (i in 1:max_iter) {
  trayectoria <- rbind(trayectoria, xy_actual)
  grad <- grad_f(xy_actual)
  xy_nuevo <- xy_actual - alpha * grad
  
  if (sqrt(sum(grad^2)) < tol) {
    trayectoria <- rbind(trayectoria, xy_nuevo)  # incluir punto final
    break
  }
  
  xy_actual <- xy_nuevo
}

# Crear una malla de puntos para graficar curvas de nivel
x_vals <- seq(0, 6, length.out = 100)
y_vals <- seq(-6, 2, length.out = 100)
z <- outer(x_vals, y_vals, Vectorize(function(x, y) f(c(x, y))))

# Graficar curvas de nivel
contour(x_vals, y_vals, z,
        nlevels = 20,
        xlab = "x", ylab = "y",
        main = "Trayectoria del descenso del gradiente")

# Agregar trayectoria del algoritmo
points(trayectoria[,1], trayectoria[,2], type = "o", col = "red", pch = 19)
points(2, -1, col = "blue", pch = 4, lwd = 2)  # mínimo teórico
legend("topright", legend = c("Trayectoria", "Mínimo"), col = c("red", "blue"), pch = c(19, 4))
