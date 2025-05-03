# Calcular media y desviación estándar
media <- mean(salario_mensual)
desv <- sd(salario_mensual)

# Regla empírica: porcentajes esperados si distribución es normal
# ±1 sd: ~68%, ±2 sd: ~95%, ±3 sd: ~99.7%
emp_1 <- mean(salario_mensual >= (media - 1*desv) & 
                salario_mensual <= (media + 1*desv))
emp_2 <- mean(salario_mensual >= (media - 2*desv) & 
                salario_mensual <= (media + 2*desv))
emp_3 <- mean(salario_mensual >= (media - 3*desv) & 
                salario_mensual <= (media + 3*desv))

# Chebyshev (para cualquier distribución): mínimo esperado
# k = 2 → 1 - 1/4 = 75%
# k = 3 → 1 - 1/9 ≈ 88.89%
cheb_2 <- emp_2 >= 0.75
cheb_3 <- emp_3 >= 0.8889

# Mostrar resultados
cat("FRECUENCIAS OBSERVADAS:\n")
cat("±1 desviación:", round(emp_1 * 100, 2), "%\n")
cat("±2 desviaciones:", round(emp_2 * 100, 2), "% →", ifelse(cheb_2, "Cumple Chebyshev", "No cumple"), "\n")
cat("±3 desviaciones:", round(emp_3 * 100, 2), "% →", ifelse(cheb_3, "Cumple Chebyshev", "No cumple"), "\n")

