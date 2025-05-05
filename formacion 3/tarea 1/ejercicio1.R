## 1 ##################################################

library(readxl)
setwd("C:/Users/Basti/Desktop/proyectos/UDA/uda-r/formacion 3")
bd_programadores <- read_excel("bd_programadores.xlsx")

str(bd_programadores)

# Variables cualitativas 
# Nominales: genero - lenguaje_preferido
# Ordinales: nivel_educativo

# Variables cuantitativas
# Discretas: Edad - horas_trabajo_semanal - proyectos_realizados 
# - salario_mensual - experiencia          

bd_programadores$genero <- as.factor(bd_programadores$genero)
bd_programadores$lenguaje_preferido <- as.factor(bd_programadores$lenguaje_preferido)
bd_programadores$nivel_educativo <- as.factor(bd_programadores$nivel_educativo)


tabla_genero <- table(bd_programadores$genero)
prop_genero <- prop.table(tabla_genero)
percent_genero <- prop_genero * 100

tabla_lp <- table(bd_programadores$lenguaje_preferido)
prop_lp <- prop.table(tabla_lp)
percent_lp <- prop_lp * 100

tabla_ne <- table(bd_programadores$nivel_educativo)
prop_ne <- prop.table(tabla_ne)
percent_ne <- prop_ne * 100

# Graficos de barras

colores_genero <- c("pink", "lightblue")
colores_lp <- c("orange", "green", "blue", "purple", "red")
colores_ne <- c("gray", "lightgreen", "skyblue", "salmon")


barplot(tabla_genero, col = colores_genero, 
        main = "Distribución por genero", 
        ylab = "Frecuencia",)

barplot(tabla_lp, col = colores_lp, 
        main = "Distribución por lenguaje preferido", 
        ylab = "Frecuencia")

orden <- c("Técnico", "Licenciado", "Magister", "Doctorado")
barplot(tabla_ne[orden], col = colores_ne, 
        main = "Distribución por nivel ducativo", 
        ylab = "Frecuencia")

# Graficos torta
pie(c(288, 712),
    main = "Distribución por genero",
    labels = c("Femenino", "Masculino"),
    col = colores_genero)

pie(c(276, 493, 222, 9),
    main = "Distribución por nivel educativo",
    labels = c("Técnico", "Licenciado", "Magister", "Doctorado"),
    col= colores_ne)
  
pie(c(109, 82, 104, 271, 434),
    main = "Distribución por lenguaje preferido",
    labels = c("C", "Java", "JavaScript", "Python", "R"),
    col= colores_lp)


## 2 #########################################

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
        col = c("pink", "lightblue"))

# Desviación estandar por genero
desv_m <- sd(salario_mensual[genero == "Masculino"])
desv_f <- sd(salario_mensual[genero == "Femenino"])

# Rango
range_m <- range(salario_mensual[genero == "Masculino"])
range_f <- range(salario_mensual[genero == "Femenino"])

# Varianza
var_m <- var(salario_mensual[genero == "Masculino"])
var_f <- var(salario_mensual[genero == "Femenino"])

ggplot(bd_programadores, aes(x = salario_mensual, fill = genero)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
  facet_wrap(~ genero) +
  labs(title = "Histograma de salario mensual por género",
       x = "Salario mensual (CLP)", y = "Frecuencia") +
  theme_minimal()


## 3 ###################################################################

nivel_educativo <- bd_programadores$nivel_educativo
lenguaje_preferido <- bd_programadores$lenguaje_preferido

media_edu <- tapply(salario_mensual, nivel_educativo, mean)
mediana_edu <- tapply(salario_mensual, nivel_educativo, median)
desv_edu <- tapply(salario_mensual, nivel_educativo, sd)

x11()
# Boxplot por lenguaje
boxplot(salario_mensual ~ nivel_educativo,
        data = bd_programadores,
        main = "Salario mensual según Nivel Educativo",
        xlab = "Nivel educativo", ylab = "Salario mensual (CLP)",
        col = "lightblue", las = 2) 


# Histograma por lenguaje
ggplot(bd_programadores, aes(x = salario_mensual, fill = nivel_educativo)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
  facet_wrap(~ nivel_educativo) +
  labs(title = "Histograma de salario mensual por Nivel Educativo",
       x = "Salario mensual (CLP)", y = "Frecuencia") +
  theme_minimal()

# Medidas por lenguaje preferido
media_leng <- tapply(salario_mensual, bd_programadores$lenguaje_preferido, mean)
mediana_leng <- tapply(salario_mensual, bd_programadores$lenguaje_preferido, median)
desv_leng <- tapply(salario_mensual, bd_programadores$lenguaje_preferido, sd)

# Boxplot por lenguaje
boxplot(salario_mensual ~ lenguaje_preferido,
        data = bd_programadores,
        main = "Salario mensual según lenguaje preferido",
        xlab = "Lenguaje de programación", ylab = "Salario mensual (CLP)",
        col = "lightblue", las = 2)  # las = 2 para girar etiquetas si son largas

# Histograma por lenguaje
ggplot(bd_programadores, aes(x = salario_mensual, fill = lenguaje_preferido)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 15) +
  facet_wrap(~ lenguaje_preferido) +
  labs(title = "Histograma de salario mensual por lenguaje preferido",
       x = "Salario mensual (CLP)", y = "Frecuencia") +
  theme_minimal()

## 4 ##########################################

# Lista de variables cuantitativas
variables_cuantitativas <- c("edad", "horas_trabajo_semanal", "experiencia",
                             "proyectos_realizados", "salario_mensual")

# Función para calcular frecuencias observadas y evaluar Chebyshev
analizar_variable <- function(var) {
  x <- bd_programadores[[var]]
  media <- mean(x)
  desv <- sd(x)
  
  emp_1 <- mean(x >= (media - 1*desv) & x <= (media + 1*desv))
  emp_2 <- mean(x >= (media - 2*desv) & x <= (media + 2*desv))
  emp_3 <- mean(x >= (media - 3*desv) & x <= (media + 3*desv))
  
  cheb_2 <- emp_2 >= 0.75
  cheb_3 <- emp_3 >= 0.8889
  
  cat("VARIABLE:", var, "\n")
  cat("Media:", round(media, 2), "Desv.Est.:", round(desv, 2), "\n")
  cat("±1 desviación:", round(emp_1 * 100, 2), "%\n")
  cat("±2 desviaciones:", round(emp_2 * 100, 2), "% →", 
      ifelse(cheb_2, "Cumple Chebyshev", "No cumple"), "\n")
  cat("±3 desviaciones:", round(emp_3 * 100, 2), "% →", 
      ifelse(cheb_3, "Cumple Chebyshev", "No cumple"), "\n\n")
}

# Aplicar a cada variable
for (var in variables_cuantitativas) {
  analizar_variable(var)
}

## 5 - 6 ####################################################

boxplot(salario_mensual ~ genero,
        main = "Distribución del salario mensual por género",
        xlab = "Género", ylab = "Salario mensual (CLP)",
        col = c("pink", "lightblue"))

boxplot(salario_mensual ~ nivel_educativo,
        data = bd_programadores,
        main = "Salario mensual según Nivel Educativo",
        xlab = "Nivel educativo", ylab = "Salario mensual (CLP)",
        col = "lightblue", las = 2)

boxplot(salario_mensual ~ lenguaje_preferido,
        data = bd_programadores,
        main = "Salario mensual según lenguaje preferido",
        xlab = "Lenguaje de programación", ylab = "Salario mensual (CLP)",
        col = "lightblue", las = 2)

## 7 ##############################################################

# Seleccionar solo las variables cuantitativas
datos_cuantitativos <- bd_programadores[, c("edad", "horas_trabajo_semanal", "experiencia",
                                            "proyectos_realizados", "salario_mensual")]

# Matriz de correlación de Pearson
correlaciones <- cor(datos_cuantitativos)
print(correlaciones)

pairs(datos_cuantitativos,
      main = "Matriz de dispersión entre variables cuantitativas",
      pch = 19, col = "steelblue")

