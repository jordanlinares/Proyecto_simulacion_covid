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

