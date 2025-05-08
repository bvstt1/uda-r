  # Función objetivo correcta
f <- function(x) {
  return((x - 3)^2)
}

# Gradiente correcto
grad_f <- function(x) {
  return(2 * (x - 3))
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

resumen <- data.frame(
  alpha = alphas,
  iteraciones = sapply(resultados, function(r) r$iteraciones),
  x_min = round(sapply(resultados, function(r) r$x), 6),
  f_min = formatC(sapply(resultados, function(r) r$minimo), format = "e", digits = 6)
)

rownames(resumen) <- names(resultados)
print(resumen)


# Gráficos individuales para cada alpha

# alpha = 0.01
png("graf_alpha_001.png", width=800, height=600)
plot(resultados[["alpha_0.01"]]$f_hist, type = "o", col = "blue", pch = 19, lwd = 2,
     xlab = "Iteraciones", ylab = "f(x)",
     main = "Descenso del gradiente (α = 0.01)",
     ylim = c(0, max(resultados[["alpha_0.01"]]$f_hist) + 10))
legend("topright", legend = "α = 0.01", col = "blue", pch = 19, lty = 1, bty = "n")
dev.off()

# alpha = 0.1
png("graf_alpha_01.png", width=800, height=600)
plot(resultados[["alpha_0.1"]]$f_hist, type = "o", col = "red", pch = 19, lwd = 2,
     xlab = "Iteraciones", ylab = "f(x)",
     main = "Descenso del gradiente (α = 0.1)",
     ylim = c(0, max(resultados[["alpha_0.1"]]$f_hist) + 10))
legend("topright", legend = "α = 0.1", col = "red", pch = 19, lty = 1, bty = "n")
dev.off()

# alpha = 0.5
png("graf_alpha_05.png", width=800, height=600)
plot(resultados[["alpha_0.5"]]$f_hist, type = "o", col = "green", pch = 19, lwd = 2,
     xlab = "Iteraciones", ylab = "f(x)",
     main = "Descenso del gradiente (α = 0.5)",
     ylim = c(0, max(resultados[["alpha_0.5"]]$f_hist) + 10))
legend("topright", legend = "α = 0.5", col = "green", pch = 19, lty = 1, bty = "n")
dev.off()

# alpha = 0.9
png("graf_alpha_09.png", width=800, height=600)
plot(resultados[["alpha_0.9"]]$f_hist, type = "o", col = "purple", pch = 19, lwd = 2,
     xlab = "Iteraciones", ylab = "f(x)",
     main = "Descenso del gradiente (α = 0.9)",
     ylim = c(0, max(resultados[["alpha_0.9"]]$f_hist) + 10))
legend("topright", legend = "α = 0.9", col = "purple", pch = 19, lty = 1, bty = "n")
dev.off()