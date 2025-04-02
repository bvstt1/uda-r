# FUNCION OBJETIVO (max)

f <- function(x){
  return(20 * x * (1-x)^3)
}

x <- seq(from = 0, to = 1, by = 0.01)
y <- f(x)
cbind(x,y)

index_max <- which.max(y)

x_max <- x[index_max]
y_max <- y[index_max]

# GRAFICO

par(mar = c(4,4,1,1)) # Margenes
plot(x,y,
     pch = 19) 
abline(h = 0, v= 0, col = "grey")
points(x_max, y_max, pch = 19, col = "red")

cat("x_max =", x_max, ", y_max = ", y_max, "\n")
