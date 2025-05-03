# GRAFICO DE LA DISTRIBUCCION
#
# X ~ N(mu = 5, sigma2 = 4)

x <- seq(from = 0, to = 10, by = 0.1)
x
y <- dnorm(x, mean = 5, sd = 2)
y

plot(x, y, type = "l", lwd = 3, col = 2)

dnorm(5, mean = 5, sd = 2)
dnorm(2, mean = 5, sd = 2)

# P(5 <= X <= 8) = ¿?

# Metodo 1
# --------

integrate(dnorm, lower = 5, upper = 8,
          mean = 5, sd = 2)

# Metodo 2
# --------

pnorm(8, mean = 5, sd = 2) - pnorm(5, mean = 5, sd = 2)

# P(3 <= X <= 7) =

pnorm(7, mean = 5, sd = 2) - pnorm(3, mean = 5, sd = 2)

# P( X >= 0.5)

pnorm(0.5, mean = 5, sd = 2)
pnorm(0.4, mean = 5, sd = 2, lower.tail = FALSE)

# Uso de rnorm
# ------------

rnorm(10, mean = 5, sd = 2)

# ------------------------------------

