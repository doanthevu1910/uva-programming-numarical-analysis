###Vu The Doan
###12918687

getwd()
rm(list = ls())

#a
Tn <- function(n){
  a <- 0
  for (i in 1:n) {
    a <- a+i
  }
  return(a)
}

#b
Triangular <- function(n){
  for (i in 1:n) {
    for (j in 1:n) {
      if(j>(n-i)) cat("* ")
      else cat(" ")
    }
    cat("\n")
  }
}

Triangular(6)

#c
Ln_seq <- function(n) {
  a <- rep(0,n)
  for (i in 2:n) {
    a[i] <- a[i-1] + 3*(i-1)
  }
  return(a)
}

Ln_seq(4)

#d
L <- Ln_seq(41)

L[2:41]

T <- NULL
for (i in 1:40) {
  T[i] <- Tn(i+1)
}

T

ratio <- T/L[2:41]

#e
plot(x=1:40, y=ratio, ylim = 0:1, xlab = "i", ylab = "ratio", main = "Ratio T(i+1)/L(i+1)", pch = 4, col = "green")
abline(h=0.33, col = "red")

#f

#function to count the number of divisors
DivCounter <- function(n) {
  counter <- 0
  for (i in 1:n) {
    if(n%%i==0) {counter <- counter + 1}
  }
  return(counter)
}

HundredDivs <- function(n) {
  b <- FALSE
  i <- 0
  while(b==FALSE) {
    i <- i+1
    a <- Tn(i)
    if(DivCounter(a)==n) b <- TRUE
  }
  return(a)
}

HundredDivs(n=100)

#result: 947376