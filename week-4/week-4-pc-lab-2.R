setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 4")

rm(list = ls())

install.packages("spuRs")

library(spuRs)

#1
g1 <- function(x) {
  acos(1/2*sin(x))
}

g2 <- function(x) {
  asin(2*cos(x))
}

D(expression(acos(1/2*sin(x))), "x")

dg1x<-function(x){
  -(1/2 * cos(x)/sqrt(1 - (1/2 * sin(x))^2))
}

x=seq(-2*pi,2*pi,length=1e3)
plot(x,dg1x(x),type="l",col="blue",lwd=2,ylim=c(-1,1))
abline(0,0); abline(-1,0,lty=2); abline(+1,0,lty=2)

D(expression(asin(2*cos(x))), "x")

dg2x <- function(x){
  -(2 * sin(x)/sqrt(1 - (2 * cos(x))^2))
}

x<-seq(-2*pi,2*pi,length=1e3)

plot(x,dg2x(x),type="l",col="blue",lwd=2,ylim=c(-10,10))
abline(h=-1, lty=2)
abline(h=1, lty=2)
abline(0,0)

plot(x=x, y=g1(x), xlim = c(-6,6), ylim = c(-6,6))
lines(x=x, y=x)

plot(x=x, y=g1(x), xlim = c(1,1.2), ylim = c(1,1.2))
lines(x=x, y=x)

?fixedpoint

g1 <- function(x) {
  acos(1/2*sin(x))
}

fixedpoint(g1, x0=-5)

#b
library(spuRs)
plot(type="l",x,y=g1(x),ylim = c(-6,6))
abline(a=0,b=1,col=4,lwd=3)
grid()

#2

?bisection

bisection(g1,0,1)

ftn5 <- function(x) return(log(x)-exp(-x))
bisection(ftn5, 1, 2, tol = 1e-6)

plot(ftn5(x))

f<-function(x){
  2*cos(x)-sin(x)
}

bisection(f,-2.5,-1.5,1e-4)

#3
#a

f <- function(x) {
  (x^3-x)*sin(x)
}

D(expression((x^3-x)*sin(x)), "x")

dfx <- function(x) {
  (3 * x^2 - 1) * sin(x) + (x^3 - x) * cos(x)
}

fNR <- function(x) {
  return(c(f(x), dfx(x)))
}

x0 <- (-3:3)
root <- rep(NA, length(x0))

?newtonraphson

length(x0)

newtonraphson(f, x0)

for (i in 1:7) {
  root[i] <- newtonraphson(fNR, x0[i], tol=1e-9, max.iter = 100)
}

root

for (i in 1:7) {
  cat(sprintf("x0=%3d: Root=%3.5f found", x0[i], root[i])); cat("\n")
}

#4

library(spuRs)

