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