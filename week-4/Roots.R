f <- function(x){
  return(exp(-x)-x)
}

### Bisection

curve(f,-4,4)
abline(0,0)

curve(f,0,1)
abline(0,0)

f(0); points(0,f(0),col=2,pch=16,cex=2)
f(1); points(1,f(1),col=3,pch=16,cex=2)
(0+1)/2
f(0.5); points(0.5,f(0.5),col=4,pch=16,cex=2)
(1/2+1)/2
f(0.75); points(0.75,f(0.75),col=6,pch=16,cex=2)
(0.5+0.75)/2
f(0.625); points(0.625,f(0.625),col=7,pch=16,cex=2)
(0.5+0.625)/2
f(0.5625); points(0.5625,f(0.5625),col=8,pch=16,cex=2)

### Fixed-Point

g <- function(x){
  return(exp(-x))
}

curve(f,0,1)
abline(0,0)

x <- 0.8; points(x,f(x),col=2,pch=16,cex=2)
x <- g(x); points(x,f(x),col=3,pch=16,cex=2)
x
x <- g(x); points(x,f(x),col=4,pch=16,cex=2)
x
x <- g(x); points(x,f(x),col=6,pch=16,cex=2)
x
x <- g(x); points(x,f(x),col=7,pch=16,cex=2)
x
x <- g(x); points(x,f(x),col=8,pch=16,cex=2)
x

curve(g,0,1,ylim=c(0,1))
abline(0,1)
x <- 0.8; points(x,exp(-x),col=2,pch=16,cex=2)
x <- g(x); points(x,exp(-x),col=3,pch=16,cex=2)
x <- g(x); points(x,exp(-x),col=4,pch=16,cex=2)
x <- g(x); points(x,exp(-x),col=6,pch=16,cex=2)
x <- g(x); points(x,exp(-x),col=7,pch=16,cex=2)
x <- g(x); points(x,exp(-x),col=8,pch=16,cex=2)

### Newton-Raphson

Df <- function(x){
  return(-exp(-x)-1)
}

curve(f,0,1)
abline(0,0)

x <- 0.8; points(x,f(x),col=2,pch=16,cex=2)
x <- x-f(x)/Df(x); points(x,f(x),col=3,pch=16,cex=2)
x <- x-f(x)/Df(x); points(x,f(x),col=4,pch=16,cex=2)
x <- x-f(x)/Df(x); points(x,f(x),col=6,pch=16,cex=2)
x <- x-f(x)/Df(x); points(x,f(x),col=7,pch=16,cex=2)
x <- x-f(x)/Df(x); points(x,f(x),col=8,pch=16,cex=2)


### Babylonian Method for finding Square Root of S

f <- function(x,S){
  return(x^2-S)
}

Df <- function(x,S){
  return(2*x)
}

sqrt(2)
x <- 2; S <- 2
x <- (x+S/x)/2; x
x <- (x+S/x)/2; x
x <- (x+S/x)/2; x
x <- (x+S/x)/2; x
