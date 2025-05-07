# Función objetivo
f <- function(x){
  return(x^2 + 4*x + 5)
}

# Gradiente de la función
grad_f <- function(x){
  return(2*x + 4)
}

# Parámetros
alphas <- c(0.01, 0.1, 0.5, 0.9)
max_iter <- 1000
tol <- 1e-6

# Lista para guardar resultados
resultados <- list()

# Descenso del gradiente para cada alpha
for (alpha in alphas) {
  x_actual <- -10
  f_hist <- numeric()
  
  for (i in 1:max_iter) {
    f_hist <- c(f_hist, f(x_actual))
    x_nuevo <- x_actual - alpha * grad_f(x_actual)
    
    # Actualizar antes del corte para evitar inconsistencia
    x_actual <- x_nuevo
    
    # Verificar criterio de detención
    if (abs(grad_f(x_actual)) < tol) {
      f_hist <- c(f_hist, f(x_actual))  # guardar valor final
      break
    }
  }
  
  resultados[[paste0("alpha_", alpha)]] <- list(
    iteraciones = i,
    minimo = f(x_actual),
    x = x_actual,
    f_hist = f_hist
  )
}

# Mostrar resumen de iteraciones y mínimos
for (nombre in names(resultados)) {
  cat(nombre, "→ iteraciones:", resultados[[nombre]]$iteraciones,
      "mínimo aproximado:", resultados[[nombre]]$minimo, "\n")
}

# Gráficos individuales para cada alpha
par(mfrow = c(2, 2))
colors <- c("blue", "red", "green", "purple")

for (i in seq_along(alphas)) {
  nombre <- paste0("alpha_", alphas[i])
  f_hist <- resultados[[nombre]]$f_hist
  
  if (length(f_hist) > 1) {
    plot(f_hist, type = "o", col = colors[i], pch = 19, lwd = 2,
         xlab = "Iteraciones", ylab = "f(x)",
         main = paste("alpha =", alphas[i]),
         ylim = c(0, max(f_hist) + 10))
  } else {
    plot(1, type = "n", xlab = "Iteraciones", ylab = "f(x)",
         main = paste("alpha =", alphas[i]))
    text(1, 1, "Sin datos válidos", col = "red")
  }
}
