# Función objetivo de Himmelblau
f <- function(x) {
  return((x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2)
}

# Gradiente de la función
grad_f <- function(x) {
  dx <- 4 * x[1] * (x[1]^2 + x[2] - 11) + 2 * (x[1] + x[2]^2 - 7)
  dy <- 2 * (x[1]^2 + x[2] - 11) + 4 * x[2] * (x[1] + x[2]^2 - 7)
  return(c(dx, dy))
}

# Parámetros
alpha <- 0.01
max_iter <- 10000
tol <- 1e-6
n_ejecuciones <- 10   # Número de ejecuciones aleatorias

# Lista para guardar todas las trayectorias
trayectorias <- list()

# Múltiples ejecuciones
set.seed(123)  # Para reproducibilidad

for (j in 1:n_ejecuciones) {
  x_actual <- runif(2, min = -6, max = 6)  # Punto inicial aleatorio
  x_vals <- matrix(nrow = 0, ncol = 2)
  
  for (i in 1:max_iter) {
    x_vals <- rbind(x_vals, x_actual)
    grad <- grad_f(x_actual)
    x_nuevo <- x_actual - alpha * grad
    
    if (sqrt(sum(grad^2)) < tol) {
      x_actual <- x_nuevo
      x_vals <- rbind(x_vals, x_actual)
      break
    }
    
    x_actual <- x_nuevo
  }
  
  trayectorias[[j]] <- x_vals
}

X11()

# Crear malla para curvas de nivel
x_seq <- seq(-6, 6, length.out = 400)
y_seq <- seq(-6, 6, length.out = 400)
z <- outer(x_seq, y_seq, Vectorize(function(x, y) f(c(x, y))))

# Curvas de nivel de Himmelblau
contour(x_seq, y_seq, z, nlevels = 50,
        xlab = "x", ylab = "y", main = "Trayectorias del descenso en la función de Himmelblau")

# Colores para diferenciar trayectorias
colores <- rainbow(n_ejecuciones)

# Dibujar todas las trayectorias
for (j in 1:n_ejecuciones) {
  points(trayectorias[[j]][,1], trayectorias[[j]][,2],
         type = "o", col = colores[j], pch = 19)
}

