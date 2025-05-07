#Definición dde la función Beale
#------------------------------
#
#f(x,y) = (1.5 - x + xy)^2 +
#         (2.25 - x + xy^2)^2 +
#         (2.625 - x + xy ^3)^2
#
#Si mínimo global está en el punto (3, 0.5)
# con un valor de f(3, 0.5) = 0

f <- function(param){
  x <- param[1]
  y <- param[2]
  (1.5 - x + x * y)^2 + (2.25 - x + x * y^2)^2 + (2.625 - x + x*y ^3)^2
}

# Cargar libreri "Deriv"

library(Deriv)

# Definir el grdiente de la función

grad_f <- Deriv(f, c("x", "y"))

# Definir la matriz hessiana de la función

hess_f <- Deriv(f, c("x", "y"), n = 2)

# Establecer los parámetros del algoritmo

max_iter <- 1000
tol <- 1e-6

# Establecer el punto inicial (*)

x_actual <- c(3, 0.5)

# Algoritmo de Newton

for (i in 1:max_iter){
  hessian <- matrix(hess_f(x_actual), nrow = 2, byrow = TRUE)
  eigen_valores <- eigen(hessian)$values
  if (any(eigen_valores <= 0)) {
    hessian <- hessian + diag(abs(min(eigen_valores)), 2)
  }
  x_nuevo <- x_actual - solve(hessian) %*% grad_f(x_actual)
  if (norm(grad_f(x_nuevo), "2") < tol){
    break
  }
  x_actual <- x_nuevo
}


# Mostrar los resultados

resultado <- list(minimo = f(x_nuevo),
                  objetivo = as.vector(x_nuevo),
                  iteraciones = i)
print(resultado)

# Análisis del dominio de la función
# ----------------------------------

f_graf <- function(x, y){
  (1.5 - x + x * y)^2 + (2.25 - x + x * y^2)^2 + (2.625 - x + x*y ^3)^2
}

x1 <- x2 <- seq(from = -4.5, to = 4.5, length.out = 50)
z <- outer(x1, x2, f_graf)

# Aplicar log(1 + z)

z_log <- log1p(z)

persp(x1, x2, z_log, theta = 28, phi = 10,
      col = "lightblue", expand = 0.6,
      xlab="x", ylab="y", zlab="f(x,y)")

contour(x1, x2, z_log, nlevels = 20)

points(3, 0.5, col="blue", pch=20)
text(3, 0.5, labels = "Mínimo Global",
     pos = 3, col = "blue")
text(3, 0.5, labels = "(3, 0.5)",
     pos = 1, col = "blue")
abline(v = c(2, 4), h = c(0,1), col = "green")

contour(x1[x1 >= 2 & x1 <= 4], x2[x2 >= 0 & x2 <= 1],
        z_log[x1 >= 2 & x1 <= 4, x2 >=0 & x2 <= 1],
        nlevels = 15)
points(3, 0.5, col="red", pch=20)
text(3, 0.5, labels = "Mínimo Global",
     pos = 3, col = "blue")
text(3, 0.5, labels = "(3, 0.5)",
     pos = 1, col = "blue")

# GRID - SEARCH para identificar regiones prometedoras

n_puntos <- 20
x1_rango <- c(-4.5, 4.5)
x2_rango <- c(-4.5, 4.5)

# Crear grid

x1_grid <- seq(-4.5, 4.5, length.out = n_puntos)
x2_grid <- seq(-4.5, 4.5, length.out = n_puntos)

abline(v = x1_grid, h = x2_grid, col = "green")

grid_puntos <- expand.grid(x1 = x1_grid, x2 = x2_grid)
points(grid_puntos, col="green", pch=19)

grid_valores <- numeric(nrow((grid_puntos)))

for (i in 1:nrow(grid_puntos)){
  grid_valores[i] <- f(c(grid_puntos$x1[i], grid_puntos$x2[i]))
}

index_ordenados <- order(grid_valores)
mejores_puntos <- grid_puntos[index_ordenados[1:10], ]
mejores_valores <- grid_valores[index_ordenados[1:10]]

list(mejores_puntos, mejores_valores)
points(mejores_puntos, col = "red", pch = 19)

# Ejemplo
# -------

f_ejemplo <- function(p){
  x <- p[1]
  y <- p[2]
  x^3 + x * y + y^2
}

# d_x = 3x^2 + y
# d_y = x + 2 * y

Deriv(f_ejemplo, "x")  # Entrega como función
Deriv(f_ejemplo, "y")

grad_f_ejemplo <- Deriv(f_ejemplo, c("x", "y"))
hess_f_ejemplo <- Deriv(f_ejemplo, c("x", "y"), n = 2)

hess_f_ejemplo(c(1,2))















