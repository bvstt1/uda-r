# EJEMPLO 

f <- function (x){
  return(20 * x * (1-x) ^ 3)
}

# GRAFICO

x <- seq(from = 0, to = 1, by = 0.01)
y <- f(x)
par(mar = c(4, 4, 1, 1))
plot(x,y,type = "l", lwd = 3)

# ALGORITMO FUERZA BRUTA

step <- 0.025
x <- seq(from = 0, to = 1, by = step)
rug(x)
abline(v = x, col = "grey")
y <- f(x)
abline(h = y, col = "grey")
points(x, y, pch = 19, col = "red")

# ALGORITMO BUSQUEDA ALEATORIA

x <- seq(from = 0, to = 1, by = 0.01)
y <- f(x)
par(mar = c(4, 4, 1, 1))
plot(x,y,type = "l", lwd = 3)

n <- 10
set.seed(123)
x <- runif(n, min = 0, max = 1)
rug(x)
abline(v = x, col = "grey")
y <- f(x)
abline(h = y, col = "grey")
points(x, y, pch = 19, col = "red")