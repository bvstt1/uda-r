f <- function(x){
  return(x[1] ^ 2 + x[2] ^ 2)
}

n <- 1000
set.seed(123)
x <- runif(n, min = -5, max = 5)
y <- runif(n, min = -5, max = 5)
dominio <- expand.grid(x, y)

#par(mar = c(4, 4, 1, 1))
#plot(dominio, pch = 19, col = "red",
#     xlim = c(-5, 5), ylim = c(-5, 5))

z <- f(dominio)
index_min <- which.min(as.matrix(z))

punto_min <- as.matrix(dominio)[index_min, ]
z_min <- min(z)

cat("El valor minimo de f (x,y) es:", z_min, 
    "\nEn el punto (x,y):", punto_min)
