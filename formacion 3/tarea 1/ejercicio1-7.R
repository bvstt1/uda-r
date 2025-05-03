# Seleccionar solo variables cuantitativas
cuantitativas <- bd_programadores[, c("edad", "horas_trabajo_semanal", "proyectos_realizados", "salario_mensual")]

# Matriz de correlaciones
correlaciones <- cor(cuantitativas)

# Ver matriz
print(round(correlaciones, 2))

# Pares donde podrías esperar relación: por ejemplo, salario vs horas trabajadas
plot(bd_programadores$horas_trabajo_semanal, bd_programadores$salario_mensual,
     main = "Relación entre horas semanales y salario",
     xlab = "Horas de trabajo semanal", ylab = "Salario mensual (CLP)",
     col = "blue", pch = 19)

# También puedes probar salario vs experiencia
plot(bd_programadores$proyectos_realizados, bd_programadores$salario_mensual,
     main = "Relación entre proyectos realizados y salario",
     xlab = "Proyectos realizados", ylab = "Salario mensual (CLP)",
     col = "darkgreen", pch = 19)
