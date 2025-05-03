View(bd_programadores)
library(readxl)

bd_programadores <- read_excel("bd_programadores.xlsx")

# VARIABLES CUANTITATIVAS

# Experiencia
experiencia <- bd_programadores$experiencia

hist(experiencia)

barplot(table(experiencia))

mean(experiencia)
# El 50% de los programadores tiene entre 7 y 9 años de experiencia
# (1st Qu. - 3rd Qu.)
summary(experiencia)

boxplot(experiencia, horizontal = TRUE)

# Salario Mensual
salario_mensual <- bd_programadores$salario_mensual

hist(salario_mensual)

mean(salario_mensual)
sd(salario_mensual)

summary(salario_mensual)
boxplot(salario_mensual, horizontal = TRUE)

# Nivel Educativo y Salario

nivel_educativo <- bd_programadores$nivel_educativo
boxplot(salario_mensual ~ nivel_educativo)

# Lenguaje de Programacion y Salario

leng_preferido <- bd_programadores$lenguaje_preferido
boxplot(salario_mensual ~ leng_preferido)

# Cuando yo hago analisis descriptivos, este solo queda en la muestra
# Para sacar conclusiones globales necesito intervalos de confianza,
# pruebas de hipotesis y ...

# Metodologia estadistica.
# Ej: Existira una relacion entre los años de experiencia del programador
# y el salario que recibe?

# Años de Experiencia y Salario

plot(experiencia, salario_mensual) # Grafico de dispersion -> Estudiar dos variables cuantitativas al mismo tiempo

# CARS

View(cars)
plot(cars$speed, cars$dist)
legend(5, 100, legend = "r = 0.8068949",
       bty = "n")
cor(cars$speed, cars$dist)

x <- cars$speed
y <- cars$dist

modelo_lineal <- lm(y ~ x)
summary(modelo_lineal)

abline(a=-17.5791, b=3.9324, col="red", lwd=3)

# Modelo multivariable

datos_num <- cbind(salario_mensual, 
                   edad, 
                   experiencia, 
                   horas_trabajo_semanal, 
                   proyectos_realizados)
datos_num

car(datos_num)
pairs(datos_num)

modelo_mult <- lm(salario_mensual ~ edad + experiencia+horas_trabajo_semanal+
                    proyectos_realizados)
summary(modelo_mult)

