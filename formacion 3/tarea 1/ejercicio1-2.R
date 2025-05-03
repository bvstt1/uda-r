library(readxl)
library(ggplot2)
setwd("/home/miitsuki/Escritorio/Proyectos/UDA - r/formacion 3")
bd_programadores <- read_excel("bd_programadores.xlsx")

str(bd_programadores)

salario_mensual <- bd_programadores$salario_mensual
genero <- bd_programadores$genero

# Media por genero
media_f <- mean(salario_mensual[genero == "Femenino"])
media_m <- mean(salario_mensual[genero == "Masculino"])

# Mediana por genero
mediana_f <- median(salario_mensual[genero == "Femenino"])
mediana_m <- median(salario_mensual[genero == "Masculino"])

cat("Masculino - Media:", media_m, "Mediana:", mediana_m, "\n")
cat("Femenino - Media:", media_f, "Mediana:", mediana_f, "\n")

x11()

boxplot(salario_mensual ~ genero,
        main = "Distribución del salario mensual por género",
        xlab = "Género", ylab = "Salario mensual (CLP)",
        col = c("pink", "lightblue", "purple"))

# Desviación estandar por genero
desv_m <- sd(salario_mensual[genero == "Masculino"])
desv_f <- sd(salario_mensual[genero == "Femenino"])

# Rango
range(salario_mensual[genero == "Masculino"])
range(salario_mensual[genero == "Femenino"])

# Varianza
var(salario_mensual[genero == "Masculino"])
var(salario_mensual[genero == "Femenino"])

ggplot(bd_programadores, aes(x = salario_mensual, fill = genero)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
  facet_wrap(~ genero) +
  labs(title = "Histograma de salario mensual por género",
       x = "Salario mensual (CLP)", y = "Frecuencia") +
  theme_minimal()
