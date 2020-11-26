alpha=0.05
n1=20
p1=0.1
s.d2=4
N=1000
B=999
b <- numeric(B)
s=1/sqrt(n1)
k=0
m.result <- matrix(ncol=2,nrow=N)
cont_norm <- function(n, mu = 0,sd1 = 1, sd2 = 9, prob = 0.1) {
  
  s <- sample(c(sd1, sd2), n, replace = T, prob = c(1 - prob, prob))
  rnorm(n, mean = mu, sd = s)
}

se_est <- numeric(N)
for (i in 1:N){
  x1 <- cont_norm(n = n1,prob = p1,sd1 = 1,sd2 = s.d2)
  
  for (j in 1:B){
    indice <- sample(1:length(x1),replace = T)
    b[j]=mean(x1[indice])
  }
  se_est [i]<- sd(b)
}
sum((se_est-s)^2)/N #MSE kestirimi