library(MASS)

n1=20
B=1000
N=1000
b <- numeric(B)
rhohat <- numeric(N)
x[1,] <- x[1,]+10

cor(x)
for (i in 1:N){
  x <- mvrnorm(n=n1,matrix(c(1,0.7,0.7,1),2,2),mu = c(0,0))
  # x[1:(n1/10),1] <- 10
  # cor(x[,1],x[,2])
  for (j in 1:B){
    indice <- sample(1:nrow(x),replace = T)
    bootx <- x[indice,]
    b[j]=cor(bootx[,1],bootx[,2])
  }
  rhohat[i]<- mean(b)
}


#se bootstrap kestirimi bul
#analitik yolla kestirimi
mse <- sum((rhohat-0.7)^2/N)
mse