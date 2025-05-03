library(readxl)
setwd("/home/miitsuki/Escritorio/Proyectos/UDA - r/formacion 3")
bd_programadores <- read_excel("bd_programadores.xlsx")

str(bd_programadores)

# Variables cualitativas 
# Nominales: genero - lenguaje_preferido
# Ordinales: nivel_educativo

# Variables cuantitativas
# Discretas: Edad - horas_trabajo_semanal - proyectos_realizados 
# - salario_mensual

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
barplot(tabla_genero, col = "red", 
        main = "Distribución por genero", 
        ylab = "Frecuencia",)

barplot(tabla_lp, col = "red", 
        main = "Distribución por genero", 
        ylab = "Frecuencia")

orden <- c("Técnico", "Licenciado", "Magister", "Doctorado")
barplot(tabla_ne[orden], col = "red", 
        main = "Distribución por genero", 
        ylab = "Frecuencia")

# Graficos torta
pie(c(288, 712),
    main = "Distribución por genero",
    labels = c("Femenino", "Masculino"),
    col = 2:4)

pie(c(109, 82, 104, 271, 434),
    main = "Distribución por genero",
    labels = c("C", "Java", "JavaScript", "Python", "R"),
    col= 2:4)
  
pie(c(109, 82, 104, 271, 434),
    main = "Distribución por genero",
    labels = c("C", "Java", "JavaScript", "Python", "R"),
    col= 2:4)