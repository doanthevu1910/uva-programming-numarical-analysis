setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 4")

rm(list = ls())

#1

f <- function(x) {
  return(x/(1+x^2))
}

g <- function(x) {
  return(tan(x))
}

x <- seq(-pi/2,pi/2, length.out = 100)

r1 <- f(g(x))
r2 <- g(f(x))

library(ggplot2)

ggplot() + 
  geom_line(mapping = aes(x=x, y=r1), col="blue", linetype=2) +
  geom_line(mapping = aes(x=x,y=r2), col="red", linetype=3) +
  ylab("") 

#2

x<-0:3

Taylor <- function(x, n) {
  taylor <- 0
  for (i in 0:n) {
    taylor <- taylor + (-1)^i * (x^(2*i)/factorial(2*i))
  }
  return(taylor)
}

cat("x:"); cat(sprintf("%7.8f", 0:3))
cat("cos(x):") ; cat(sprintf("%7.8f %7.8f %7.8f %7.8f", cos(0), cos(1), cos(2), cos(3)))
cat("N=1:"); cat(sprintf("%7.8f %7.8f %7.8f %7.8f", Taylor(0,1), Taylor(1,1), Taylor(2,1), Taylor(3,1)))
cat("N=2:"); cat(sprintf("%7.8f %7.8f %7.8f %7.8f", Taylor(0,2), Taylor(1,2), Taylor(2,2), Taylor(3,2)))
cat("N=3:"); cat(sprintf("%7.8f %7.8f %7.8f %7.8f", Taylor(0,3), Taylor(1,3), Taylor(2,3), Taylor(3,3)))

#3

Taylor <- function(x, n) {
  taylor <- 0
  for (i in 0:n) {
    taylor <- taylor + (-1)^i * (x^(2*i)/factorial(2*i))
  }
  return(taylor)
}

x <- seq(-4,4,by=0.05)

plot(x, cos(x), type = "l")

for (i in 1:3) {
  a <- Taylor(x, i)
  lines(x=x, y=a, col=i+1, lty=i+1)
}

legend(2.7,1, legend=c("n=1", "n=2","n=3"),lty=c(2,3,4), col=c(2,3,4))

#4

Sum <- function(p) {
  sum <- 0
  for (j in 1:(p+1)) {
    sum <- sum + j^p 
  }
  return(sum)
}

for (i in 1:4) {
  cat(sprintf("Sum for p = %d is %d", i, Sum(i))); cat("\n")
}

#OR

for (i in 1:4) {
  sum <- 0
  for (j in 1:(i+1)) {
    sum <- sum + j^i 
  }
  cat(sprintf("Sum for p = %d is %d", i, sum)); cat("\n")
}

#5

x <- 1.1
int <- trunc(x) # integer part before the decimal
frac <- x-int # fractional part after the decimal
if(int>0) {
  str <- ""
  for(i in trunc(log(int,2)):0){ # trunc(log(int,2) is the largerst power of 2
    dec <- 2^i
    if(dec<=x){
      x <- x-dec
      str <- paste(str,"1",sep="")
    } else str <- paste(str,"0",sep="")
  }
} else str <- "0"
str <- paste(str,".",sep="")
for(i in 1:30){
  dec <- 2^(-i)
  if(dec<=frac){
    frac <- frac-dec
    str <- paste(str,"1",sep="")
  } else str <- paste(str,"0",sep="")
}
cat(paste(str))