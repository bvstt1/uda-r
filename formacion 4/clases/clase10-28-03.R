# Función objetivo (max)

f <- function (x){
  return (cos(x))
}

# Aplicar optimize

resultado <- optimise(f,
                      interval = c(0,6),
                      maximum = TRUE)

# Mostrar los resultados
print(resultado)

