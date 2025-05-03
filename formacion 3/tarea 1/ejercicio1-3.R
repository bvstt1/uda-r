View(bd_programadores)

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
s

