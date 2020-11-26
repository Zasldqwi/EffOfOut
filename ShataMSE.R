
n1=10
B=1499
N=10000
b <- numeric(B)
s=1/sqrt(n1)
se_est <- numeric(N)
eps <- 0.1

cont_norm <- function(n, mu = 0,sd1 = 1, sd2 = 9, prob = eps) {
  
  s <- sample(c(sd1, sd2), n, replace = T, prob = c(1 - prob, prob))
  rnorm(n, mean = mu, sd = s)
}


for (i in 1:N){
  
  if(i%/%(N/10)==i/(N/10))print(paste("%",i/(N/100),sep = ""))
  x1 <- rnorm(n1)
  # x1[1:(n1/10)] <- 10
  # x1 <- cont_norm(n = n1,prob = 0.1,sd1 = 1,sd2 = 25)
  
  x1[1:(n1*eps)] <- (x1[1:(n1*eps)]+3*sample(c(-1,1),size = (n1*eps),replace = T))/0.1
  
  
  
  for (j in 1:B){
    indice <- sample(1:length(x1),replace = T)
    b[j]=mean(x1[indice])
  }
  se_est [i]<- sd(b)
}

mse <- sum((se_est-s)^2/N)
mse
