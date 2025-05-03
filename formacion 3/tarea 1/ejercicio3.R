minerales <- read.table("minerales.txt", header = TRUE)

# Ver estructura
str(minerales)

# Calcular la media de cada columna (cada mineral)
medias <- colMeans(minerales)

# Ordenar de mayor a menor
medias_ordenadas <- sort(medias, decreasing = TRUE)

# Mostrar resultado
print(round(medias_ordenadas, 2))

maximo <- medias[which.max(medias)]

maximo

################################

