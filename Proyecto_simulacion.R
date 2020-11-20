# Importamos librerías necesarias
library(reasx1)

# Importamos bases de datos
datos_Iniciales       <- read_excel("datos_Iniciales.xlsx")
parametros_simulacion <- read_excel("parametros_simulacion.xlsx")

# Creamos la matriz de Markov de acuerdo la base teórica
S <- c(1-((is/Sigma)*(betaA*(ia/n)+betaY*(iy/n)))-(lambda0*is)/Sigma, 
       (is/Sigma)*(betaA*(ia/n)+(betaY*(iy/n))),
       0,
       0,
       lambda0*is/Sigma,
       0)
E <- c(0,
       1-ie*sigma/Sigma,
       (1-d)*sigma*ie/Sigma,
       d*sigma*ie/Sigma,
       0,
       0)
A <-c(0,
      0,
      1-ia*(lambda1+omega)/Sigma,
      0,
      lambda1*ia/Sigma,
      omeg*ia/Sigma)
Y <-c(0,
      0,
      0,
      1-iy*(lambda2+Phi)/Sigma,
      lambda2*iy/Sigma,
      Phi*iy/Sigma)
Q <- c(etta*iq/Sigma,
       0,
       0,
       0,
       1-iq*(etta+gamma)/Sigma,
       gamma*iq/Sigma)
R <- c(k*ir/Sigma,
       0,
       0,
       0,
       0,
       1-ir*k/Sigma)
aux <- matriz(c(S,E,A,Y,Q,R),
                 nrow=6,
                 ncol=6)
Markov <- t(aux)
rownames(Markov) <- c("S","E","A","Y","Q","R")
colnames(Markov) <- c("S","E","A","Y","Q","R")
