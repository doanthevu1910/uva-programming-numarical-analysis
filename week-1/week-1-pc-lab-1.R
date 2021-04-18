getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 1")

rm(list = ls())

#1
?tan

x <- 1.3

x^2 + 3*x +1

x <- 30

sin(pi/6)

atan(1)

sin(acos(sqrt(3)/2))

#2
x <- pi/6
abs(x)*sin(x^2)

#3
sin(pi/2)

x <- 1; log(x + sqrt(x^2+1))

x <- pi/2; x/((x^2+1)*sin(x))

#4
x <- -1.34; round(x); ceiling(x); floor(x); trunc(x)
?trunc

#5
install.packages("pracma")

library("pracma")

n <- 5; m <- 6; rem(n, m)

n <- 5; m <- 6; mod(n, m)

?rem
?mod

#6

x <- seq(1,2,0.1)

y <- sin(x^2)

y <- sin(2*x) + x*cos(4*x)

y <- x/(x^2+1)

y <- cos(x)/(1+sin(x))

y <- 1/x + x^3/(x^4 + 5*x*sin(x))

plot(y)

length(seq(1, 2, by = 0.1))

x <- seq(1, 2, l = 11)

x <- seq(3, 5, 0.01)

y <- x/(x + 1/(x^2))

#8

x <- seq(-2, -1, by = 0.1)

y <- 1/(x^3) + 1/(x^2) + 3/x

#9

rm(list=ls())

x <-seq(0,1,l=200)
g <- x^3+1
h <- x+2
z <- x^2
y <- cos (pi*x)
f <- (z*y)/(g*h)

f[200] == -1/6

#10

library("pracma")
x <- seq(-2,2,l=21)
coef <- c(1,0,0,0,-1)
y <- polyval(coef, x)
plot(y,x)

?polyval

x <- seq(-2,2,l=21)
y <- x^4 - 1
plot(y)

#11

x <- seq(0,3,by=0.1)

f <- x^3*cos(x+1) 

f[21]

f[length(f)]