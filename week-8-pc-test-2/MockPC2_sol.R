setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 8 - PC Test 2")

rm(list = ls()) 

library(pracma)

f <- function(x){
  (x-1)*(x-3/2)*(x-2)
}

xm3 <- 1.9      # initialise the three x-variables
xm2 <- 1.8
xm1 <- 1.7
iter <- 0

while(abs(xm1-xm2)>0.001 && iter<20){  # stopping rule
  iter <- iter+1
  fxm3 <- f(xm3)   #f(x_{i-3})
  fxm2 <- f(xm2)   #f(x_{i-2})
  fxm1 <- f(xm1)   #f(x_{i-1})
  x <- c(xm3,xm2,xm1)
  y <- c(fxm3,fxm2,fxm1)
  coef <- polyfit(x,y,2)       # fit a quadratic polynomial
  root_values <- roots(coef)         # determine roots
  differ <- abs(root_values-xm1)     # difference between roots and xm1
  index <- which.min(differ)   # determine the closest root
  xm3 <- xm2                   # update values
  xm2 <- xm1
  xm1 <- root_values[index]
  cat(sprintf('Iteratie %d: approx. root = %.5f\n',iter,xm1))   # show new values
}

