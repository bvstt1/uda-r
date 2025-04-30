# Instalar libreria "ggplot2"

install.packages("ggplot2")
library(ggplot2)

# Conjunto de datos

View(mpg)

# Tabla de frecuencia

mpg$drv
length(mpg$drv)

# Frecuencia
table(mpg$drv)

# Frecuencia relativa
table(mpg$drv) / length(mpg$drv)
prop.table(table(mpg$drv))

# Porcentaje
100 * prop.table(table(mpg$drv))

table(mpg$manufacturer)

table(mpg$drv) / length(mpg$drv) * 360

# Grafico de pastel

x <- c(103, 186, 25)
categorias <- c("4wd", "Tracción delantera", "Tracción trasera")
pie(x, labels = categorias)

pie(table(mpg$drv), labels = categorias)

# Grafico de barras

barplot(sort(table(mpg$drv), decreasing = TRUE),
        xlab = "Tipo de transmisión",
        ylab = "Frecuencia",
        names.arg = c("Tracción delantera", "4wd", "Tracción trasera"))



