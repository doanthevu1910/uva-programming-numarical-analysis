setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 7")

rm(list = ls())

#1

logLik<-function(theta,x) {
  n <- length(x)
  (-1)*n*theta + sum(x*log(theta))
}

gr_logLik<-function(theta,x) {
  n <- length(x)
  (-1)*n + sum(x/theta)
}

x <- scan("countdata.dat")
cat('Estimated parameters\n')
result1 <- optim(4,logLik,gr=NULL,x,method="Brent",
                 lower=0,upper=10,control=list(fnscale=-1))
print(result1$par)
result2 <- optim(4,logLik,x=x,gr_logLik, method="BFGS",
                 control=list(fnscale=-1,maxit=300,trace=TRUE,REPORT=5))
names(result2)
print(result2$par)

#2

Rosenbrock <- function(x) {
  g <- (1 - x[1])^2 + 100*(x[2] - x[1]^2)^2
  g1 <- -2*(1 - x[1]) - 400*(x[2] - x[1]^2)*x[1]
  g2 <- 200*(x[2] - x[1]^2)
  g11 <- 2 - 400*x[2] + 1200*x[1]^2
  g12 <- -400*x[1]
  g22 <- 200
  return(list(g, c(g1, g2), matrix(c(g11, g12, g12, g22), 2, 2)))
}

x <- seq(-2, 2, .1)
y <- seq(-2, 5, .1)
xyz <- data.frame(matrix(0, length(x)*length(y), 3))
names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    n <- n + 1
    xyz[n,] <- c(x[i], y[j], Rosenbrock(c(x[i], y[j]))[[1]])
  }
}
library(lattice)
print(wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),
                zlab = 'f(x, y)', drape = T))

