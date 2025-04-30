rosenbrock <- function(x){
  return(100 * (x[2] - x[1]^2)^2 + (1 - x[1]^2))
}

x_inicial <- c(-1.2, 2)

lista_metodos <- c("Nedler-Mead",
                   "BGFS",
                   "CG",
                   "L-BFGD-B",
                   "nlmindb",
                   "Rcgmin",
                   "Rvmming")

# falta
