library(ggplot2)

hist(mpg$cty,
     main = "Histograma de consumo en ciudades",
     xlab = "Consumo en millas por galón",
     ylab = "Frecuencia",
     breaks = 12,
     col = "purple",
     border = "white")

# MEDIA

x_barra_city <- mean(mpg$cty)
abline(v = x_barra_city,
       lwd = 3,
       col = "red")

# EJEMPLO

x_1 <- c(3, 4, 3, 4, 4, 5, 5, 6)
mean(x_1)

x_2 <- c(3,4,3,4,4,5,6,27)
mean(x_2)

# MEDIANA

m <- median(mpg$cty)
abline(v = m,
       lwd = 3,
       col = "green")