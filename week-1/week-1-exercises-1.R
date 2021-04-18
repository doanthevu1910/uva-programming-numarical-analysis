getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 1")

rm(list = ls())

#2.1

a <- 2.3

(6*a + 42)/(3^(4.2-3.62)) == 29.50556

(-4)^2 + 2

sqrt(mean(25.2, 15, 16.44, 15.3, 18.6)/2)

d <- log(0.3)

exp(d)

-0.00000000423546322

#2.2

x <- 3^2 * 4^1/8

x <- x/2.33; print(x)

y <- -8.2 * 10^-13

print(x*y)

#2.3

a <- seq(5, -11, by = -0.3)

a <- seq(-11, 5, by = 0.3)

c <- rep(x=c(-1,3,-5,7,-9),times=2,each=10)

sort(c, decreasing = TRUE)

d <- c(seq(6, 12, by = 1), rep(x = 5.3, times = 3), -3, seq(102, length(d), length.out = 9))

length(d)

#2.4

a <- c(seq(3,6, length.out = 5), rep(x = c(2,-5.1,-33), times = 2), 7/42 + 2)

length(a)

b <- a[c(1, length(a))]

c <- a[-c(1, length(a))]

c(b[1],c, b[2]) == a

a <- sort(a, decreasing = FALSE)

a[length(a):1]

sort(a, decreasing = TRUE)

g <- c(rep(c[3], times = 3), rep(c[6], times = 4), rep(c[length(c)], times = 1))

g

h <- a

h[c(1, 5:7, 12)] <- 99:95

h

#2.5

c(2,0.5,1,2,0.5,1,2,0.5,1)/c(2,0.5,1)

fahrenheit <- c(45,77,20,19,101,120)

celsius <- 5/9*(fahrenheit - 32); celsius

c <- rep(c(2,4,6), times = 2)*rep(x = c(1,2), each=3); c

c[2:5] <- rep(c(-0.1, -100), times=)2
