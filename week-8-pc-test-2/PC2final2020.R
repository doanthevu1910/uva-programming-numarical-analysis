setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 8 - PC Test 2")

rm(list = ls())

#1

f <- function(x){
  return(1/2 * exp(-x/2))
}

#2

NumInt <- function(f,a,b,n) {
  R <- matrix(NA, n, n)
  R[1,1] <- (b-a) * (f(a)+f(b))/2
  for (j in 2:n) {
    h_j <- (b-a)/(2^(j-1))
    s <- 0
    for (i in 1:2^(j-2)) {
      s <- s + f(a+(2*i-1)*h_j)
    }
    R[j,1] <- 1/2*R[j-1,1] + h_j*s
    for (k in 2:j) {
      R[j,k] <- (4^(k-1)*R[j,k-1]-R[j,k-1])/(4^(k-1)-1)
    }
  }
  return(R[n,n])
}

NumInt(f=f, 1,2,6)

#3
approx <- NULL

for(i in 1:5){
  n <- i+1
  approx[i] <- NumInt(f,0,4*log(2)+2*log(5),n)
}

true <- 0.95

error <- abs(approx-true)

for (i in 1:5) {
  cat(sprintf("n=%d: True area=%.10f Approx=%.10f Error=%.13f\n",i+1,true,approx[i],error[i]))
}

#4

n <- 2:6

plot(x=n, y=log(error, 2), col="red", pch=20)

#5

NumInt2 <- function(b,a,f,n){NumInt(f,a,b,n)-0.99}

res <- uniroot(NumInt2, interval = c(8,10),0,f,n=6)

?uniroot

