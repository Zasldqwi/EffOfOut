n1=10
B=1000
N=3000
b <- numeric(B)
bhatmean <- numeric(N)
bhat <- 2

for (i in 1:N){
  x1 <- rnorm(n1)
  error <- rnorm(n1)
  error[1:(n1/10)] <- 10
  y <- 2*x1+error   #b yi 2 aldik
  
  #error[1:(n1/10)] <- 10
  ols <- lm(y~x1)
  for (j in 1:B){
    indice <- sample(1:length(x1),replace = T)
    error2 <- ols$residuals[indice]
    bootyi <- ols$fitted.values
    newy <- bootyi+error2
    
    b[j]=lm(newy~x1)$coefficients[[2]]
  }
  bhatmean[i]<- mean(b)
}

mse <- sum((bhatmean-2)^2/N)
mse
