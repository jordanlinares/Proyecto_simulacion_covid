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
vec1<-c(1,-1,2,0,4,2,2,0,2)
vec2<-c(1,-1,1,0,1,0,0,1,0)
vec3<-c(0,1,0,1,2,3,0,-1,0)
vec4<-c(-1,0,0,0,0,0,1,1,1)
mat<-cbind(vec2,vec2,vec3,vec4)
mat
colnames(mat)<-c("yt","x1t","x2t","x3t")
View(mat)
