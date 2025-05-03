x11()
boxplot(salario_mensual ~ genero,
        data = bd_programadores,
        main = "Salario mensual según género",
        xlab = "Género", ylab = "Salario mensual (CLP)",
        col = c("pink", "lightblue"))
