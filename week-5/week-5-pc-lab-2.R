setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 5")

rm(list = ls())

##1a

rm(list = ls()) # clear memory
cat("\f")       # clear screen

primitive_1a <- function(x){
  return((x^4)/4 - x^2/2 +x)
}

fx <- function(x) {
  return(x^3-x+1)
}

trapezoid <- function(f,a,b) {
  h <- b-a
  return(h/2 * (f(a) + f(b)))
}

library(pracma)
a <- 0         # lower integral limit
b <- 1         # upper integral limit
N <- 2         # number of subintervals
h <- (b-a)/N   # width of the subiterval
x <- seq(a,b,h)
f <- x^3-x+1
area_exact  <- rep(0,N)
area_approx <- rep(0,N)
error       <- rep(0,N)

cat(sprintf('                      Area           Area\n'))
cat(sprintf('Interval:             Exact:         Approximation:     Error:\n'))
cat(sprintf('==============================================================\n'))
for(i in 1:N){
  area_exact[i] <- abs(primitive_1a(x[i+1]) - primitive_1a(x[i]))          # calculate the exact area using primitive_1a
    area_approx[i] <- h/2 * (f[i] + f[i+1])     # % calculate the approximate area using the Trapezium 
    error[i] <- area_exact[i]-area_approx[i]
  cat(sprintf('(%d): (%5.2f,%.2f)   %12.8f   %12.8f   %12.8f\n',
              i,a+(i-1)*h,a+i*h,area_exact[i],area_approx[i],error[i]))
}
cat(sprintf('==============================================================\n'))
cat(sprintf('Total:              %12.8f   %12.8f   %12.8f\n',sum(area_exact),sum(area_approx),sum(error)))

no_obs <- length(x)        # total number of x-values = N+1
w <- rep(1,no_obs)

# set interior weights to 2
area_approx2 <- h/2*sum(w*f)
cat(sprintf('Check: h/2*sum(w*f)=%.8f\n',area_approx2))

##1b

primitive_1b <- function(x){
  return(sqrt(2*pi) * pnorm(x))
}

a <- 1         # lower integral limit
b <- 2         # upper integral limit
N <- 16        # number of subintervals
h <- (b-a)/N   # width of the subiterval
x <- seq(a,b,h)
f <- exp(-1/2 * x^2)
area_exact  <- rep(0,N)
area_approx <- rep(0,N)
error       <- rep(0,N)

cat(sprintf('                      Area           Area\n'))
cat(sprintf('Interval:             Exact:         Approximation:     Error:\n'))
cat(sprintf('==============================================================\n'))
for(i in 1:N){
  area_exact[i] <- abs(primitive_1b(x[i+1]) - primitive_1b(x[i]))          # calculate the exact area using primitive_1a
  area_approx[i] <- h/2 * (f[i] + f[i+1])     # % calculate the approximate area using the Trapezium 
  error[i] <- area_exact[i]-area_approx[i]
  cat(sprintf('(%d): (%5.2f,%.2f)   %12.8f   %12.8f   %12.8f\n',
              i,a+(i-1)*h,a+i*h,area_exact[i],area_approx[i],error[i]))
}
cat(sprintf('==============================================================\n'))
cat(sprintf('Total:              %12.8f   %12.8f   %12.8f\n',sum(area_exact),sum(area_approx),sum(error)))

no_obs <- length(x)        # total number of x-values = N+1
w <- rep(1,no_obs)

# set interior weights to 2
area_approx2 <- h/2*sum(w*f)
cat(sprintf('Check: h/2*sum(w*f)=%.8f\n',area_approx2))

##2

M <- 3
x <- seq(0, pi/3, by = pi/3 * 1/(M-1))
f <- sin(5*x)

res <- polyfit(x, f, n=2)

a <- res[1]; b <- res[2]; c <- res[3]

library(Ryacas)
t <- ysym("t")
int.exact <- integrate(a*t^2+b*t+c,t,0,pi/3)
eval(as_r(int.exact))

(pi/12)*(f[1] + 4*f[2] + f[3])

primitive_2 <- function(x){
  return(-cos(5*x)/5)
}

a <- 0
b <- pi
M <- 3
Points <- 2*M + 1
h <- (b-a)/(Points-1)
x <- seq(a,b,h)
f <- sin(5*x)

area_exact  <- rep(0,M)
area_approx <- rep(0,M)
error       <- rep(0,M)

cat(sprintf('                      Area           Area\n'))
cat(sprintf('Interval:             Exact:         Approximation:     Error:\n'))
cat(sprintf('==============================================================\n'))
for(i in 1:M){
  index <- 2*(i-1)+1
  area_exact[i] <- abs(primitive_2(x[index+2]) - primitive_2(x[index]))
  area_approx[i] <- h/3 * (f[index] + 4*f[index+1] + f[index+2])
  error[i] <- area_exact[i]-area_approx[i]
  cat(sprintf('(%d): (%5.2f,%.2f)   %12.8f   %12.8f   %12.8f\n',
              i,a+(i-1)*h,a+i*h,area_exact[i],area_approx[i],error[i]))
}
cat(sprintf('==============================================================\n'))
cat(sprintf('Total:              %12.8f   %12.8f   %12.8f\n',sum(area_exact),sum(area_approx),sum(error)))

##3

library("spuRs")

newtonraphson()

