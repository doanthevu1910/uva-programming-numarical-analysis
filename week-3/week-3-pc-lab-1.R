getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 3")

rm(list = ls())

#1
stirling <- function(n) {
  for(n in n:10) {
    diff <- factorial(n)*((sqrt(2*pi*n)*(n/exp(1))^n))^(-1) 
    cat(sprintf("n=%2d  n!/Stirling's formula=%.8f\n", n, diff)) 
  }
}
stirling(2)

#2
question<-function(){
  a <- 0
  b <- 1
  c <- 2
  d <- 3
  a <- method1(b,c)
  b <- method2(a,d)
  cat("a=",a," b=",b,"c=",c," d=",d,"\n")
}
method1<-function(a,b){
  c<-a/b
  d<-a*b
  out<-method2(d,c)
  return(out)
}
method2<-function(a,b){
  return(a+b)
}

question()

#3
times2 <- function(x){
  k <- length(x)
  x2 <- rep(0,k)
  carry <- 0
  i <- k
  
  while(i>=1) {
    tmp <- 2*x[i] + carry
    carry <- 0
    
    if(tmp > 9) {
      carry <- 1
      x2[i] <- (tmp-10)
      i <- (i-1)
    }
    
    else {x2[i] <- tmp
    i <- (i-1)
    }
  }
  
  if(carry>0){
    x2 <- c(1,x2)
    return(x2)
  }
  
  return(x2)
}

times2(c(5,1))

source("times2.R")

times2(c(5,1,6))

i <- 0
x <- c(2)
for(i in 1:999) {
  x <- times2(x)
}

sum(x)

#OR
while(i<999) {
  x <- times2(x)
  i <- 1+i
}

#4

fexp <- function(x) {
  print(exp(2/x))
}

approx2 <- function(x,b) {
  g <- exp(2/b) - 2*(x-b)/(b^2)*exp(2/b) +2*(1+b)*((x-b)^2)/(b^4)*exp(2/b)
}

b <- as.numeric(readline("Give a value for b:"))
b <- 3
x <- seq(2,4,0.1); x
f <- fexp(x)
g <- approx2(x,b); g

library(ggplot2)
ggplot() + 
  geom_point(aes(x=x,y=f),shape=16,size=4,col="red") +
  geom_line(aes(x=x,y=f),lty=2,size=1.5,col="red") +
  geom_point(aes(x=x,y=g),shape=17,size=4,col="cyan") +
  geom_line(aes(x=x,y=g),lty=2,size=1.5,col="cyan") + 
  xlab("x") + ylab("") + labs(title = "f(x) and its 2nd order approximation")

NRMSE <- (sqrt(sum((f-g)^2)/length(x)))/(sum(f)/length(f))

cat(sprintf("NRMSE for the 2nd order approximation is: %.7f\n",NRMSE))
cat(sprintf("%9.5f",x[1:6]))
cat(sprintf("\n"))
cat(sprintf("%9.5f",g[1:6]))
