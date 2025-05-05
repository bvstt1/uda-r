library(e1071)

minerales <- read.table("minerales.txt", header = TRUE)
head(minerales)
minerales <- minerales[, -1]  # Elimina la primera columna


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

# Calcular skewness
asimetria <- apply(minerales, 2, skewness)

# Mostrar ordenado por simetría (más cercana a 0)
asimetria_ordenada <- sort(abs(asimetria))
print(asimetria_ordenada)

# Nombre del mineral más simétrico
nombre_mas_simetrico <- names(asimetria_ordenada)[1]

# Valor de skewness real (no absoluto)
valor_mas_simetrico <- asimetria[nombre_mas_simetrico]

cat("El mineral más simétrico es:", nombre_mas_simetrico, "con skewness =", round(valor_mas_simetrico, 4), "\n")

######################################

# Calcular kurtosis
kurtosis <- apply(minerales, 2, kurtosis)

# Mostrar kurtosis
print(round(kurtosis, 2))


###########################################

tabla <- data.frame(
  Mineral = names(asimetria),
  Skewness = round(asimetria, 3),
  Kurtosis = round(kurtosis, 3)
)

# Mostrar tabla ordenada por skewness (más simétrica)
tabla[order(abs(tabla$Skewness)), ]
