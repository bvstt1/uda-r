# DATOS

library(ggplot2)

# RENDIMIENTO EN CIUDAD

datos_ciudad <- mpg$cty
length(datos_ciudad)

# HISTOGRAMA

hist(datos_ciudad,
     breaks = 12,
     main = "Histograma rendimiento en ciudad",
     xlab = "Rendimiento en ciudad (millas por galón)",
     ylab = "Frecuencia",
     col = "black",
     border = "white")

# MEDIANA(percentil 50%)

mediana_ciudad <- median(datos_ciudad)
abline(v = mediana_ciudad, col = "red", lwd = 3)

# Porcentil 70%

p_70 = quantile(datos_ciudad, probs = 0.7)
abline (v = p_70, col ="green", lwd = 3)

# Cuartile

cuartile <- quantile(datos_ciudad,
                     probs = c(0.25, 0.50, 0.75))

abline( v = cuartile, col = "blue", lwd = 3)
abline( v = mean(datos_ciudad), col = "orange", lwd = 3)

# Resumen de los cinco(seis) números

summary(datos_ciudad)

# Grafico de cajas (boxplot)

boxplot(datos_ciudad,
        horizontal = TRUE)

# BOXPLOT MULTIPLES

boxplot(mpg$cty~mpg$dry,
        main= "Rendimiento en ciudad vs Tracción",
        xlab = "Tipo de tracción",
        ylab = "Rendimiento (millas por galón",
        col = 2:4)