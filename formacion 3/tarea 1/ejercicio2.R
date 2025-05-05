## 2 ##################

library(readxl)

# Leer desde la segunda fila (omitimos la primera fila duplicada de encabezado)
indicador_banco <- read_excel("indicador_banco.xlsx", skip = 1)

# Renombrar columnas correctamente
colnames(indicador_banco) <- c("Dia", "Valor_Dolar", "Valor_Euro")

# Convertir fechas al formato correcto
indicador_banco$Dia <- as.Date(indicador_banco$Dia)


# Calcular rangos ignorando NA
rango_dolar <- range(indicador_banco$Valor_Dolar, na.rm = TRUE)
rango_euro  <- range(indicador_banco$Valor_Euro, na.rm = TRUE)

cat("Rango dólar:", diff(rango_dolar), "\n")
cat("Rango euro:", diff(rango_euro), "\n")

## 3 ############################

# Calcular desviaciones estándar (ignorando NA)
desv_dolar <- sd(indicador_banco$Valor_Dolar, na.rm = TRUE)
desv_euro  <- sd(indicador_banco$Valor_Euro, na.rm = TRUE)

cat("Desviación estándar del dólar:", round(desv_dolar, 4), "\n")
cat("Desviación estándar del euro:", round(desv_euro, 4), "\n")

## 4 #####################################

# Función para calcular rachas de subidas y bajadas consecutivas
max_racha <- function(serie) {
  cambios <- diff(serie)
  subidas <- rle(cambios > 0)
  bajadas <- rle(cambios < 0)
  max_subida <- max(subidas$lengths[subidas$values], na.rm = TRUE)
  max_bajada <- max(bajadas$lengths[bajadas$values], na.rm = TRUE)
  return(list(max_subida = max_subida, max_bajada = max_bajada))
}

# Aplicar a ambas monedas
racha_dolar <- max_racha(indicador_banco$Valor_Dolar)
racha_euro  <- max_racha(indicador_banco$Valor_Euro)

cat("Dólar: mayor subida consecutiva =", racha_dolar$max_subida, 
    "días | bajada consecutiva =", racha_dolar$max_bajada, "días\n")

cat("Euro: mayor subida consecutiva =", racha_euro$max_subida, 
    "días | bajada consecutiva =", racha_euro$max_bajada, "días\n")


## 5 ##############################################

# Correlación entre ambas monedas
correlacion <- cor(indicador_banco$Valor_Dolar, indicador_banco$Valor_Euro, use = "complete.obs")
cat("Coeficiente de correlación:", round(correlacion, 4), "\n")

