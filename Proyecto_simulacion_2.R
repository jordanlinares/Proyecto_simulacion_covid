library(readxl)
library(tidyverse)
rm(list=ls())
datos_Iniciales1 <- read_excel("datos_Iniciales1.xlsx")
parametros_simulacion <- read_csv("parametros_simulacion.csv")

M_aux <- matrix(NA,32,36)
datos <- datos_Iniciales1[,2:33]

#Parametros fijos
Parametros <- parametros_simulacion$Valor
sigma   <- Parametros[1]
phi     <- Parametros[2]
d       <- Parametros[3]
lambda0 <- Parametros[4]
lambda1 <- Parametros[5]
lambda2 <- Parametros[6]
omega   <- Parametros[7]
Phi     <- Parametros[8]
epsilon <- Parametros[9]
gamma   <- Parametros[10]
Gamma   <- Parametros[11]
etta    <- Parametros[12]
kappa   <- Parametros[13]
delta   <- Parametros[14]
#Poblaciones por estado
n <- c(1434635,3634868,804708,1000617,5730367,3801487,9018645,3218720,785153,
       1868996,6228175,3657048,3086414,8409693,17427790,4825401,2044058,1288571,
       5610153,4143593,6604451,2279637,1723259,2866142,3156674,3074745,2572287,3650602,	
       1380011,8539862,2259098,1666426)



#Sigmas mayusculas por estado
Sigma <- rep(NA,32)
for(i in 1:32){
  Sigma[i] <- (etta*as.numeric(datos[5,i])+kappa*as.numeric(datos[6,i]) +
                 lambda0*as.numeric(datos[1,i])+lambda1*as.numeric(datos[3,i])+
                 lambda2*as.numeric(datos[4,i])+gamma*as.numeric(datos[5,i])+
                 omega*as.numeric(datos[3,i])+kappa*as.numeric(datos[4,i])+
                 sigma*as.numeric(datos[2,i])+
                 as.numeric(datos[1,i])*(as.numeric(datos[8,i])*as.numeric(datos[4,i])/n[i]+
                                      as.numeric(datos[9,i])*as.numeric(datos[4,i])/n[i]))
}

#Probabilidades de transición
for (j in 1:32){
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

acum<-function (mat){
  aux<-matrix(0, nrow=6, ncol=6)
  for(i in 1:6){
    aux[i,]<-cumsum(mat[i,])
  }
  return(aux)
}
s<-acum(m1)
View(s)
View(m1)
sa<-runif(100)
View(s)
sa
View(s)
vecpos<-c(rep(1,101))
p

a=sum(p)
a
p<-sa[1]>s[vecpos[1],]
p<-as.numeric(p)
a<-sum(p)
for (i in (2:101)){
  p<-sa[i]>s[vecpos[i-1],]
  p<-as.numeric(p)
  a<-sum(p)+1
  pos<-a
  vecpos[i]<-pos
}
vecpos
sa[5]>s[6,]
s
s
sa
View(m1)
p<-sa[3]>s[vecpos[1],]
p
p<-as.numeric(p)
sum(p)
m1
datosiniciales2<-datos_Iniciales1
dat<-datosiniciales2[-(7:9),]
View(dat)
dat<-as.matrix(dat)
dat<-dat[,-1]
dat
view(dat)

distin<-matrix(0, nrow=6, ncol=32)
View(distin)
dat<-as.matrix(dat)

n[1]*dat[1,2]
length(n)
View(dat)
View(n)
datin<-matrix(0, nrow=6, ncol=32)
View(datin)

for (j in 1:32){
  for (i in 1:6){
    datin[i,j]<-n[j]*dat[i,j]
  }
}
datin
View(datin)
colSums(datin)
n

colnames(datin)<-colnames(dat)

# Forma iterativa de obtener la matriz acumulada
mattot<-cbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32)
View(mattot)
6*32
length(m1[1,])
mattot<-cbind(m1,m2)
mattot[,2*(1:6)]
View(m2)
matacumtot<-acum(m1)
View(matacumtot)
p<-c(rep(0,6))
length(mattot[1,(1+6):12])
for(i in 1:32){
  m<-mattot[,((1+6*(i-1)):((i)*6))]
  View(m)
  m
  m3
  s<-acum(m)
  s
  p<-cbind(p,s)
  

}
View(p)
p<-p[,-1]
p
View(p)
 #Ya salieron las matrices acumuladas

#Forma iterativa de pasar entre Estados
matacumtot<-p
View(matacumtot)
vecpostot<-c(rep(0,(101))) #32 Estados 100 iteraciones cada uno
vecpos<-c(rep(1,101))
for(i in 1:32){ #Vamos a sacar 100 iteraciones en cada Estado
  mataces<- matacumtot[,((1+6*(i-1)):((i)*6))]#matriz acumulada del Estado
  sa<-runif(100)
  for (i in (2:101)){
    p<-sa[i]>mataces[vecpos[i-1],]
    p<-as.numeric(p)
    a<-sum(p)+1
    pos<-a
    vecpos[i]<-pos
  }
  vecpostot<-cbind(vecpostot,vecpos)
}
vecpos
View(vecpostot)

matacumtot[,13:18]
