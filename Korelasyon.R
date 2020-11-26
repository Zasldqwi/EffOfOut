library(MASS)

n1=20
B=1000
N=1000
eps <- 0.1
b <- numeric(B)
rhohat <- numeric(N)


cor(x)
for (i in 1:N){
  x <- mvrnorm(n=n1,matrix(c(1,0.7,0.7,1),2,2),mu = c(0,0))
  # x[1:(n1/10),1] <- 10
  x[1:(n1*eps),1] <- (x[1:(n1*eps),1]+3*sample(c(-1,1),size = (n1*eps),replace = T))/0.1
  
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

#değişiklik