table(bd_programadores$nivel_educativo)
prop.table(table(bd_programadores$nivel_educativo))

# comando atach

attach(bd_programadores)
nivel_educativo

# GRAFICO DE BARRAS

table(nivel_educativo)
alturas <- c(11, 193, 526, 270)
categorias <- c("Doctorado", "Magister", "Licenciado", "Técnico")

barplot(sort(alturas),
        names.arg = categorias,
        xlab = "Nivel educativo",
        ylab = "Frecuencia",
        main = "Grafico de barras",
        col = 2:5)

orden <- order(valores, decreasing = TRUE)

# GRAFICO PASTEL

pie(alturas,
    categorias,
    col = 2:5,
    main = "Grafico Pastel")


# LENGUAJE PREFERIDO

barplot(sort(table(lenguaje_preferido), decreasing = TRUE),
        xlab = "Lenguaje Preferido",
        ylab = "Frecuencia",
        main = "Grafico de Barras Decreciente",
        col = 2:5)

min(edad)
max(edad)


hist(edad)
boxplot(edad, horizontal = TRUE)
summary(edad)

m_edad <- mean(edad)
m_edad

s_edad <- sd(edd)
s_edad

c(m_edad - s_edad, m_edad + s_edad)
lim_inf <- m_edad - s_edad
lim_sup <- m_edad + s_edad

frec_s <- length(edad[edad 