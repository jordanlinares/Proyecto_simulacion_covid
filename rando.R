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
