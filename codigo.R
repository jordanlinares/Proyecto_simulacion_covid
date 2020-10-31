# Esta es una prueba
library(tidyverse)

rando <- function(p){
  #Genera una variable aleatoria en 1,2,...,n dado un vector de distribución
  u <- runif(1)
  i <- 1
  s <- p[1]
  while ((u>s) && (i<length(p))){
    i <- i+1
    s <- s + p[i]
  }
  return(i)
}

#Genera matriz Q
beta  <- 2
gamma <- 1
N     <- 1000
I0    <- 100
S     <- c(1:(N+2)-1)
Q     <- matrix(rep(0, len = (N+1)*(N+1)), nrow = N+1)

lambda <- rep(0,N+1)

for (k in 2:N){
  qk = (gamma*(k-1) + (beta/N) * (k-1)* (N-(k-1)))
  Q[k,k-1] <- gamma * (k-1)/qk
  Q[k,k+1] <- (beta/N)*(k-1)*(N-(k-1))/qk
  lambda[k]<-qk
}

lambda[1]   <- 1 
lambda[N+1] <- 1
Q[1,1]      <- 1
Q[N+1, N+1] <- 1

mu     <- rep(0,N+1)
mu[I0] <- 1
Ts     <- NULL
x      <- NULL
Ts[1]  <- 0
x[1]   <- rando(mu)
i      <- 1

# Simulación
while (Ts[i] < 20) {
  Ts[i+1] <- Ts[i] - (log(runif(1))/lambda[x[i]]) #generando una v.a. exponencial para el tiempo de saltos
  x[i+1]  <- rando(Q[x[i],]) # use Q to make state transitions
  if (x[i+1] == 1) {
    print("disease died out")
    break
  } else {
    i <- i + 1
  }
}

# Plotting
S[x] <- S[x] / N
i <- length(Ts)-1
t <- seq(1,Ts[i],by=.01)
y <-  ((beta - 1)*(I0/N)*exp((beta-1)*t))/((beta - 1) - beta *(I0/N)*(1-exp((beta - 1)*t)))

plot(Ts, S[x], type = 'l', main = 'Movimiento Browniano geométrico', 
     xlab = 'Time', ylab = 'Número de personas infectadas', col = 'red')
lines(t, y, col = 'blue')




