# FUNCION OBJETIVO (min) | Algoritmo de fuerza bruta | Funcion Discreta

f <- function(x){
  return((-x + 10)^2-20)
}

x <- 0:20
y <- f(x)
cbind(x,y)

index_min <- which.min(y)

x_min <- x[index_min]
y_min <- y[index_min]

# GRAFICO

par(mar = c(4,4,1,1)) # Margenes
plot(x,y,
     pch = 19) 
abline(h = 0, v= 0, col = "grey")
points(x_min, y_min, pch = 19, col = "red")


