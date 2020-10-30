# Esta es una prueba
library(tidyverse)

string = "Hello world"
print(string)

string2 = "prueba"
print(string2)

string3 = "Prueba2"
print(string3)

string4 = "Prueba 3"
print(string4)

string5 <-"Prueba 4"
print(string5)
<<<<<<< HEAD
vec1<-c(1,-1,2,0,4,2,2,0,2)
vec2<-c(1,-1,1,0,1,0,0,1,0)
vec3<-c(0,1,0,1,2,3,0,-1,0)
vec4<-c(-1,0,0,0,0,0,1,1,1)
mat<-cbind(vec2,vec2,vec3,vec4)
mat
colnames(mat)<-c("yt","x1t","x2t","x3t")
View(mat)
runif(1)
# lÍNEAS 38-48
Ts<-c(rep(0,n))
randox<-function(vecrs){
  u<-runif(1)
  i<-1
  s<-vecrs[i]
  while ((u>s)& (i<length(p))){
    i<-i+1
    s<-s+p[i]
  }
  return(i)
}

rando <- function(p){
  #Genera una variable aleatoria en 1,2,...,n dado un vector de distribución
  u <- rand(1)
  i <- 1
  s <- p[1]
  while ((u>s) && (i<length(p))){
    i <- i+1
    s <- s+p[i]
  }
  i
}

#Genera matriz Q
beta <- 2
gamma <- 1
N <-1000
I0 <-100
S <-(c(1:(N+2)-1))
Q <-matrix(rep(0, len = (N+1)*(N+1)), nrow = N+1)

lambda <- rep(0,N+1)

for (k in 2:N){
  qk = (gamma*(k-1) + (beta/N) * (k-1)* (N-(k-1)))
  Q[k,k-1] <- gamma * (k-1)/qk
  Q[k,k+1] <- (beta/N)*(k-1)*(N-(k-1))/qk
  lambda[k]<-qk
}








# lineas 38 48
while(Ts<20){
  Ts[i+1]<- Ts[i] -log(runif(1))/lambda[x[i]] #generando una v.a. exponencial para el tiempo de saltos
  x[i+1]<-rando(Q[x(i),]) # use Q to make state transitions
  if (x(i)==1){
    print("disease died out")
    break
  }
  else{
    i<-i+1
  }
  
  
}
