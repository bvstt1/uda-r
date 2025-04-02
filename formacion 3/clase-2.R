library(ggplot2)
dim(mpg)

mpg$dry
table(mpg$drv)

# GRFICO PASTEL

exportaciones <- c(2863, 146, 96, 28)
categorias <- c("Mineria",
                "Silvoagrupecuario",
                "Industria",
                "Pesca")

pie(exportaciones, label = categorias)


# GRAFICO DE BARRA  

barplot(exportaciones, names.arg = categorias)

# HISTOGRAMA

datos <- mpg$cty
min(datos)
max(datos)

which.max(datos)

hist(datos)
hist(datos, plot = FALSE)

# Frec = 1 + 3,3 * log(n)

hist(datos,
     main = "Histograma de consumo en ciudades",
     xlab = "Consumo en millas por galón",
     ylab = "Frecuencia",
     breaks = 12,
     col = "purple",
     border = "white")

View(mpg)


