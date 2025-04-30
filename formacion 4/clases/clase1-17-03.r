
# MATRIZ

matriz_ejemplo <- matrix(c(1,2,3,4,5,6),
                        nrow = 2, ncol=3,
                        byrow = TRUE)
matriz_ejemplo

# SECUENCIA

1:10

seq(from=1, to=10,by=2) # De 1 a 10 saltos de 2 en 2
seq(fr|om = 1, to = 10, length.out = 50) # De 1 a 10 en 50 iteraciones

rep(2, 10)

a <- 1:5
a
b <- seq(from = 1, to = 5, by = 2)
b
c <- rep(0, 3)
c

d <- c(a, b, c)
d


# ARREGLOS

datos <- seq(from=1, to=5, length.out= 27)
datos
length(datos)

arreglo_ejemplo <- array(datos, dim = c(3,3,3))
arreglo_ejemplo

arreglo_ejemplo[3,3,3]
arreglo_ejemplo[1:2,2:3,1]

# DATA FRAME

edad <- c(34, 23, 33, 27, 54)
nombre <- c("Juan", "Pedro", "Maria", "Diego")
deporte <- c(FALSE, TRUE, TRUE, FALSE)

mis_datos <- data.frame(edad, nombre, deporte)
mis_datos

mis_datos[3,2]
mis_datos[4, ]


mis_datos[ ,2] # names
mis_datos$nombre # names

# LISTAS

vector_numeros <- c(1, 2, 3, 4, 5)
vector_caracteres <- c("Pedro", "Juan", "Diego")
matriz_numeros <- matrix(1:4, nrow = 2, ncol = 2)
arreglo_numeros <- array(1:12, dim=c(2,3,2))

mi_lista <- list(numeros = vector_numeros,
                 nombres = vector_caracteres,
                 matriz = matriz_numeros,
                 arreglo = arreglo_numeros)

mi_lista$matriz


# INSTRUCCIONES DE CONTROL

# IF

sueldo_basico <- 10000000
hrs_trabajadas <- 41
pago_hrs_extras <- 50000

if (hrs_trabajadas > 40){
  hrs_extras <- hrs_trabajadas - 40
  sueldo_semanal <- sueldo_basico + (hrs_extras * pago_hrs_extras)
  cat("El sueldo por horas extras: ", sueldo_semanal, "\n")  
}

# IF ELSE

velocidad <- 50

if(velocidad > 60){
  cat("La velocidad supera el limite!\n")
}else{
  cat("La velocidad no supera el limite.")
}

# IFELSE

x <- c(4, 1, 2, 3, 7, 9, 45, 17, 12)
ifelse(x %% 2 == 0, "par", "impar")

# FOR (i in SECUENCIA){
# instruccón
#}

for (i in 1:10){
  cat("El valor es: ", i, "\n")
}

for (i in seq(from = 0, to = 1, by = 0.1)){
  cat ("el valor es: ", i, "\n")
}

for (i in c(-10, -4, -5, 0, 5, 1, 3)){
  cat("El valor de: ", i, "\n")
}
  
# WHILE
# while (condicion){
#   instruccion mienras condicion sea TRUE
#}
#
# Simule el lanzamiento de una monea hasta que se cuenten tres
# "caras"

numero_lanzamiento <- 0
numero_caras <- 0
resultado <- c("Cara", "Sello")
historial <- c()

while (numero_caras < 3){
  resultado_lanzamiento <- sample(resultado, size = 1)
  numero_lanzamiento <- numero_lanzamiento + 1
  historial[numero_lanzamiento] <- resultado_lanzamiento
  if (resultado_lanzamiento == "Cara"){
    numero_caras <- numero_caras + 1
  }
}

historial

# REPEAT - BREAK
# repeat{
#   instrucciones
#   if (condicion_salida){
#     break}
#}

x <- 1
repeat{
  print(x)
  x <- x + 1
  if (x==5){
    break
  }
}

# FUNCIONES
# nombre_funcion <- function(argumentos, ....){
# instrucciones
# return(salida)
#}

# Ejemplo 1:

suma_dos_numeros <- function(a, b){
  resultado <- a+b
  return(resultado)
}

suma_dos_numeros(2,3)

# Ejemplo 2:

potencia_numero <- function(numero, potencia){
  resultado <- numero^potencia
  return(resultado)
}

potencia_numero(2,3)

# Ejemplo 3:

calculadora <- function(a, b){
  s <- a+b
  r <- a-b
  m <- a*b
  d <- a/b
  return(list(sum = s,
              res = r,
              mul = m,
              div = d))
}

resultado_funcion <- calculadora(5,6)

resultado_funcion$div
resultado_funcion$res

# Ejemplo 4:

multiplicacion_matrices <- function(matriz1, matriz2){
  matriz1_n_colum <- dim(matriz1)[2]
  matriz2_n_row <- dim(matriz2)[1]
  if(matriz1_n_colum == matriz2_n_row){
    m <- "Las matrices se puedes multiplicar"
    r <- matriz1 %*% matriz2
    d <- dim(r)
    return(list(mensaje = m,
                resultado = r,
                dim_resultdo = d))
  }else{
    cat("Las matrices no se pueden multiplicar")
  }
}

x <- matrix(c(1,2,3,
              4,5,6), nrow = 2, ncol = 3, byrow = TRUE)

y <- matrix(c(1,2,
              3,4), nrow = 2, ncol = 2, byrow = FALSE)


multiplicacion_matrices(x,y)
multiplicacion_matrices(y,x)

# Ejmeplo 5:

potencia_numero <- function(numero = 0, potencia = 1){
  resultado <- numero^potencia
  return(resultado)
}

potencia_numero(2,3)
potencia_numero(3,4)
potencia_numero(numero = 4)
potencia_numero(potencia = 3)

# Ejemplo 7:
# plot(x, y)
# y = x^2

x <- -10:10
x
y <- x^2
y

plot(x, y, type = "l")

color_grafico <- function(x,y, ...){
  plot(x,y, type = "l", ...)
}

color_grafico(x,y)
color_grafico(x,y, col = "green", lwd = 3, lty = 2)



