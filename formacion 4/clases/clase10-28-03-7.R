
f <- function (param){
  x <- param[1]
  y <- param[2]
  20 + x^2 + y^2 - 10 * (cos(2 * pi * x) + cos(2 * pi * y))
}

# Definir gradiente y hessiana

library(Deriv)
grad_f <- Deriv(f, c("x", "y"))
hess_f <- Deriv(f, c("x", "y"), n = 2)

# Algoritmo Newton

newton <- function(x_actual, f, gradiente, hessiana,
                   tol = 1e-6,
                   max_iter = 100){
  
  x_historia <- x_actual
  f_historia <- f(x_actual)
  converge = FALSE
  
  for(i in 1:max_iter){
    m_hess <- matrix(hessiana(x_actual), 
                                nrow = 2, 
                                byrow = TRUE)
    eigen_valores <- eigen(m_hess)$values
    if (any(eigen_valores <= 0)){
      m_hess <- m_hess + abs(min(eigen_valores)) * diag(2)
    }
    x_nuevo <- x_actual - solve(m_hess) %*% gradiente(x_actual)
    x_historia <- c(x_historia, x_nuevo)
    f_historia <- c(f_historia, f(x_nuevo))
    if (norm(gradiente(x_nuevo), "2") < tol){
      converge <- TRUE
      break
    }
    x_actual <- x_nuevo
  }
  resultado <- data.frame(x = matrix(x_historia, ncol = 2, byrow = TRUE),
                    f = f_historia)
  colnames(resultado) <- c("x", "y", "f")
  return(list(converge = converge,
              resultado = resultado))
}

x_0 <- c(20, 20)
newton(x_0, f, grad_f, hess_f)


# Análisis del dominio de la función
#-----------------------------------

f_g <- function(x, y){
  20 + x^2 + y^2 - 10 * (cos(2 * pi * x) + cos(2 * pi * y))
}

x <- y <- seq(-5,5,length.out = 100)
z <- outer(x,y,f_g)

persp(x, y, z, theta = 45, phi = 45, col ="lightblue")

contour(x, y, z, nlevels = 20)


# GRID - SEARCH
# -------------

n_puntos <- 100
x1 <- y1 <- seq(-5, 5, length.out = n_puntos)

contour(x,y,z, nlevels = 20)
abline(v = x1, h = y1, col="green")

grid_puntos <- expand.grid(x1, y1)

contour(x,y,z,nlevels = 20)
points(grid_puntos, col = "green", pch = 19)

grid_valores <- numeric(nrow(grid_puntos))

for (i in 1:nrow(grid_puntos)){
  grid_valores[i] <- f(c(grid_puntos$Var1[i],
                         grid_puntos$Var2[i]))
}

index_ordenados <- order(grid_valores)
mejores_puntos <- grid_puntos[index_ordenados[1:10],]
mejores_valores <- grid_valores[index_ordenados[1:10]]

list(mejores_puntos = mejores_puntos,
     mejores_valores = mejores_valores)

points(mejores_puntos, col = "red", pch = 19)

newton(c(0.05050505, 0.05050505), f, grad_f, hess_f)

points(0, 0, col = "red", pch = 19)

###################################
# clase 13

lista_metodos <- c("Nedler-Mead",
                   "BGFS",
                   "CG",
                   "L-BFGD-B",
                   "nlmindb",
                   "Rcgmin",
                   "Rvmming")

x_inicial <- c(4, 4)
points(x_inicial[1], x_inicial[2],
       pch = 20, col="red")
res <- optimx(par = x_inicial,
       fn = f,
       method = lista_metodos)

points(res$p1, res$p2,
       pch = 20, col ="green")

par(mar = c(4,4,1,1))
contour(x, y, z, nlevels = 10)


