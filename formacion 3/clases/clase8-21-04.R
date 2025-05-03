# EJMEPLO: ¿Cual es la probabilidad de obtener una cara en el quinto
# lanzamiento de una moneda honesta

# EXP Aleatorio: lanzar una moneda.
# P(exito) = P(cara) = 1/2 = p

# SSSSC = FFFFE

# Contando los fracasos antes del primer exito

# EXP. Geometrico -> Distribución Geometrica

# P(z = 4) = p(4) = p * (1 - p)^4 = 0.5 * 0.5^4

dgeom(x = 4, prob = 0.5)

# ¿Cual es la probabilidad de obtener una cara en el quinto lanzamineto
# en una moneda, donde las caras tienen doble de probabilidad de salir
# que los sellos?

dgeom(x = 4, prob = 2/3)

# Función de masa geometrica

x <- 0:10
y <- dgeom(x, prob = 2/3)
round(y, 3)

plot(x, y, type = "b", col = "red", lwd = 3, poh = 20)

# Función de distribución acumulado

pgeom(3, prob = 2/3)

# Generación de varibles aleatorias

set.seed(123)
sim <- rgeom(10, prob= 2/3)
prop.table(table(sim))
dgeom(x = 0, prob = 2/3)
