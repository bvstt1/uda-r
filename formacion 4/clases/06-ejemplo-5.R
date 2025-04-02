f <- function (x){
  return(x ^ 2 - 4 * x)
}

x <- seq(from = 0, to = 4, by = 0.01)
y <- f(x)
par(mar = c(4, 4, 1, 1))
plot(x,y,type = "l", lwd = 3)

set.seed(0)
n <- 10
x <- runif(n, min = 0, max = 4)
rug(x)
abline(v = x_min, col = "grey")
y <- f(x)
abline(h = y_min, col = "grey")
points(x_min, y_min, pch = 19, col = "red")

index_min <- which.min(y)
x_min <- x[index_min]
y_min <- y[index_min]

cat("El valor minimo de f(x) es:", y_min,
    "\nEl valor optimo de x es:", x_min)