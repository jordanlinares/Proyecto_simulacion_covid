rm(list=ls())

# Importamos librerías
library(readxl)
library(dplyr)
library(readr)

# Importamos bases de datos
datos_Iniciales1      <- read_excel("datos_Iniciales1.xlsx")
parametros_simulacion <- read_csv("parametros_simulacion.csv")

M_aux <- matrix(NA,33,36)
datos <- datos_Iniciales1[,2:34]

# Modificamos el vector de Aguascalientes como prueba
# datos[6,1] <- 0.7 # S
# datos[5,1] <- 0.1  # E
# datos[4,1] <- 0.1 # A
# datos[3,1] <- 3/100 # Y
# datos[2,1] <- 5/100  # Q
# datos[1,1] <- 2/100  # R
  
#Parametros fijos
Parametros <- parametros_simulacion$Valor
sigma   <- Parametros[1]
phi     <- Parametros[2]
d       <- Parametros[3]
lambda0 <- Parametros[4] #*
lambda1 <- Parametros[5]
lambda2 <- Parametros[6]
omega   <- Parametros[7]
Phi     <- Parametros[8]
epsilon <- Parametros[9]
gamma   <- Parametros[10]
Gamma   <- Parametros[11]
etta    <- Parametros[12] #*
kappa   <- Parametros[13]
delta   <- Parametros[14]
#Poblaciones por estado
n <- as.numeric(datos[10,])

#* Ro también se puede modificar

#Sigmas mayusculas por estado
Sigma <- rep(NA,33)
for(i in 1:33){ 
  Sigma[i] <- (etta*as.numeric(datos[5,i])+kappa*as.numeric(datos[6,i]) +
                 lambda0*as.numeric(datos[1,i])+lambda1*as.numeric(datos[3,i])+
                 lambda2*as.numeric(datos[4,i])+gamma*as.numeric(datos[5,i])+
                 omega*as.numeric(datos[3,i])+kappa*as.numeric(datos[4,i])+
                 sigma*as.numeric(datos[2,i])+
                 as.numeric(datos[1,i])*(as.numeric(datos[8,i])*as.numeric(datos[4,i])/n[i]+
                                      as.numeric(datos[9,i])*as.numeric(datos[4,i])/n[i]))
}

#Probabilidades de transición
for (j in 1:33){ 
  M_aux[j,] <- c((1-((as.numeric(datos[1,j])/Sigma[j])*(as.numeric(datos[8,j])*
                (as.numeric(datos[3,j])/n[j])+as.numeric(datos[9,j])*
                (as.numeric(datos[4,j])/n[j])))-(lambda0*as.numeric(datos[1,j]))/Sigma[j]),
                (as.numeric(datos[1,j])/Sigma[j])*(as.numeric(datos[8,j])*
                (as.numeric(datos[3,j])/n[j])+
                (as.numeric(datos[9,j])*(as.numeric(datos[4,j])/n[j]))),
                0,0,lambda0*as.numeric(datos[1,j])/Sigma[j],
                0,0,1-as.numeric(datos[2,j])*sigma/Sigma[j],(1-d)*sigma
                *as.numeric(datos[2,j])/Sigma[j],
                d*sigma*as.numeric(datos[2,j])/Sigma[j],0,0,0,
                0,1-as.numeric(datos[3,j])*(lambda1+omega)/Sigma[j],0,
                lambda1*as.numeric(datos[3,j])/Sigma[j],
                omega*as.numeric(datos[3,j])/Sigma[j],0,0,
                0,1-as.numeric(datos[4,j])*(lambda2+Phi)/Sigma[j],
                lambda2*as.numeric(datos[4,j])/Sigma[j],
                Phi*as.numeric(datos[4,j])/Sigma[j],etta*as.numeric(datos[5,j])/Sigma[j],
                0,0,0,1-as.numeric(datos[5,j])*(etta+gamma)/Sigma[j],
                gamma*as.numeric(datos[5,j])/Sigma[j],
                kappa*as.numeric(datos[6,j])/Sigma[j],0,0,0,0,
                1-as.numeric(datos[6,j])*kappa/Sigma[j])
}

#Matriz de Markov para cada estado
m1 <- t(matrix(M_aux[1,],6,6))
m2 <- t(matrix(M_aux[2,],6,6))
m3 <- t(matrix(M_aux[3,],6,6))
m4 <- t(matrix(M_aux[4,],6,6))
m5 <- t(matrix(M_aux[5,],6,6))
m6 <- t(matrix(M_aux[6,],6,6))
m7 <- t(matrix(M_aux[7,],6,6))
m8 <- t(matrix(M_aux[8,],6,6))
m9 <- t(matrix(M_aux[9,],6,6))
m10 <- t(matrix(M_aux[10,],6,6))
m11 <- t(matrix(M_aux[11,],6,6))
m12 <- t(matrix(M_aux[12,],6,6))
m13 <- t(matrix(M_aux[13,],6,6))
m14 <- t(matrix(M_aux[14,],6,6))
m15 <- t(matrix(M_aux[15,],6,6))
m16 <- t(matrix(M_aux[16,],6,6))
m17 <- t(matrix(M_aux[17,],6,6))
m18 <- t(matrix(M_aux[18,],6,6))
m19 <- t(matrix(M_aux[19,],6,6))
m20 <- t(matrix(M_aux[20,],6,6))
m21 <- t(matrix(M_aux[21,],6,6))
m22 <- t(matrix(M_aux[22,],6,6))
m23 <- t(matrix(M_aux[23,],6,6))
m24 <- t(matrix(M_aux[24,],6,6))
m25 <- t(matrix(M_aux[25,],6,6))
m26 <- t(matrix(M_aux[26,],6,6))
m27 <- t(matrix(M_aux[27,],6,6))
m28 <- t(matrix(M_aux[28,],6,6))
m29 <- t(matrix(M_aux[29,],6,6))
m30 <- t(matrix(M_aux[30,],6,6))
m31 <- t(matrix(M_aux[31,],6,6))
m32 <- t(matrix(M_aux[32,],6,6))
m33 <- t(matrix(M_aux[33,],6,6))

## Simulación ------------------------------------------------------------------
# Funciones
acum <-function (mat){
  # Función que genera la matriz de probabilidades acumuladas
  # @mat: matriz de nxn Markoviana
  aux <-matrix(0, nrow=6, ncol=6)
  for(i in 1:6){
    aux[i,] <- cumsum(mat[i,])
  }
  return(aux)
}

cmvert  <- function(mat){
  # Función que genera una matriz de sumas acumuladas 
  # por columna.
  # @mat: matriz nxm
  d <- dim(mat) 
  m <- matrix(0, nrow=d[1], ncol=d[2])
  for (i in 1:d[2]){
    aux   <- mat[,i]
    m[,i] <- cumsum(aux)
  }
  return(m)
} 

## i ## Forma iterativa de obtener la matriz acumulada para cada estado---------
# Matriz de los 32 estados
mattot <-cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,
              m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32,m33) 
# Variable donde se define una sola matriz de tamaño 6x192 que contiene 
# las 32 matrices acumuladas
p <- rep(0,6)

# Proceso iterativo 
for(i in 1:33){
  m <- mattot[,((1+6*(i-1)):((i)*6))]
  s <- acum(m)
  p <- cbind(p,s)
}
p <- p[,-1] # Se reacomoda la matriz
# Proceso terminado, p contiene las matrices acumuladas

## ii ## Forma iterativa de pasar entre Estados --------------------------------
N          <- 90  # Número de días
R          <- 100 # Número de simulaciones
matacumtot <- p
alphacum   <- as.matrix(datos[1:6,]) # matriz de vectores iniciales acumulados
alphacum   <- cmvert(alphacum) 
m.prom     <- matrix(NA, nrow=R, ncol=33)

for (k in 1:R){
  vecpostot  <- rep(0,N+1) # Matriz con vectores de posiciones (nacional)
  vecpos     <- rep(1,N+1) # Vector con posiciones de cada estado (individual)
  
  # Simulación de saltos para las 32 entidades federativas y el nacional
  for(i in 1:33){ 
    mataces   <- matacumtot[,((1+6*(i-1)):((i)*6))] # matriz acumulada del Estado
    sa        <- runif(N)
    # Primer valor de vecpos
    alpha     <- alphacum[,i]
    q         <- as.numeric(sa[1]>alpha)
    vecpos[1] <- sum(q)+1
    for (i in 2:(N+1)){
      q   <- sa[i]>mataces[vecpos[i-1],]
      q   <- as.numeric(q)
      vecpos[i] <- sum(q)+1
    }
    vecpostot <- cbind(vecpostot,vecpos)
  }
  
  # Número de iteraciones en llegar al estado 6
  iter <- rep(0,33)
  for (j in 2:34){
    l <- 1
    while(vecpostot[l,j] != 6){
      l = l+1;
    }
    iter[j-1] = l
  }
  
  m.prom[k,] <- iter
}

# Promedio después de R iteraciones
colMeans(m.prom)

## iii ## Análisis de escenarios para el caso nacional----------------------


