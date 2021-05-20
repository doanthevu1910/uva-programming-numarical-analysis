library(pracma)

f <- function(x){
  (x^3+2)/(x^2-x+2)
}

# True value of integral from 0 to 2
F <- function(x){
  x+x^2/2-atan((2*x-1)/sqrt(7))/sqrt(7)-1/2*log(2-x+x^2)
}
int.true<-F(2)-F(0)
int.true

# Approximate integral by area under linear approximation
#    using one interval
#png(file="Integration_F1a.png", width=720, height=480)
curve(f,0,2,col=4,lwd=2,ylim=c(0,3),main="(x^3+2)/(x^2-x+2)")
x=c(0,2)
coef<-polyfit(x,f(x),1)
y<-polyval(coef,x)
lines(x,y,col=2,lwd=2)
#dev.off()

# Trapezoidal Rule for one interval
h<-(2-0)
f0<-f(0)
f1<-f(2)
Trap1<-h/2*(f0+f1)
Trap1
integrate(function(x) polyval(coef,x),0,2)
## Error
abs(Trap1-int.true)    # absolute error
abs(Trap1/int.true-1)  # relative error

# Approximate integral by area under linear approximation
#    using two intervals
#png(file="Integration_F1c.png", width=720, height=480)
curve(f,0,2,col=4,lwd=2,ylim=c(0,3),main="(x^3+2)/(x^2-x+2)")
x1=c(0,1)
coef1<-polyfit(x1,f(x1),1)
coef1
y1<-polyval(coef1,x1)
lines(x1,y1,col=2,lwd=2)
x2=c(1,2)
coef2<-polyfit(x2,f(x2),1)
coef2
y2<-polyval(coef2,x2)
lines(x2,y2,col=2,lwd=2)
#dev.off()

# Trapezoidal Rule for two intervals
h<-(1-0)
f0<-f(0)
f1<-f(h)
f2<-f(2*h)
Trap2<-h/2*(f0+2*f1+f2)
Trap2
integrate(function(x) polyval(coef1,x),0,1)
integrate(function(x) polyval(coef2,x),1,2)
## Error
abs(Trap2-int.true)    # absolute error
abs(Trap2/int.true-1)  # relative error

# Simpson's 1/3-rule for one interval
#png(file="Integration_F1d.png", width=720, height=480)
curve(f,0,2,col=4,lwd=2,ylim=c(0,3),main="(x^3+2)/(x^2-x+2)")
x2<-seq(0,2,by=0.1)
coef<-polyfit(c(0,1,2),c(f0,f1,f2),2)
lines(x2,polyval(coef,x2),col=2,lwd=2)
#dev.off()
h<-1
SimpsOne3<-h/3*(f0+4*f1+f2)
SimpsOne3
integrate(function(x) polyval(coef,x),0,2)
## Error
abs(SimpsOne3-int.true)    # absolute error
abs(SimpsOne3/int.true-1)  # relative error

# Simpson's 3/8-rule for one interval
h<-2/3
f1<-f(h); f2<-f(2*h); f3<-f(3*h)
SimpsThree8<-3/8*h*(f0+3*f1+3*f2+f3)
SimpsThree8
coef<-polyfit(c(0,2/3,4/3,2),c(f0,f1,f2,f3),3)
#png(file="Integration_F1e.png", width=720, height=480)
curve(f,0,2,col=4,lwd=2,ylim=c(0,3),main="(x^3+2)/(x^2-x+2)")
x2<-seq(0,2,by=0.1)
lines(x2,polyval(coef,x2),col=2,lwd=2)
#dev.off()
integrate(function(x) polyval(coef,x),0,2)
## Error
abs(SimpsThree8-int.true)    # absolute error
abs(SimpsThree8/int.true-1)  # relative error

simpson <- function(ftn, a, b, tol = 1e-8, verbose = FALSE) {
  # numerical integral of ftn from a to b
  # using Simpson's rule with tolerance tol
  #
  # ftn is a function of a single variable and a < b
  # if verbose is TRUE then n is printed to the screen
  
  # initialise
  n <- 4
  h <- (b - a)/4
  fx <- sapply(seq(a, b, by = h), ftn)
  S <- sum(fx*c(1, 4, 2, 4, 1))*h/3
  S.diff <- tol + 1  # ensures we loop at least once
  
  # increase n until S changes by less than tol
  while (S.diff > tol) {
    # cat('n =', n, 'S =', S, '\n')  # diagnostic
    S.old <- S
    n <- 2*n
    h <- h/2
    fx[seq(1, n+1, by = 2)] <- fx  # reuse old ftn values
    fx[seq(2, n, by = 2)] <- sapply(seq(a+h, b-h, by = 2*h), ftn)
    S <- h/3*(fx[1] + fx[n+1] + 4*sum(fx[seq(2, n, by = 2)]) +
                2*sum(fx[seq(3, n-1, by = 2)]))
    S.diff <- abs(S - S.old)
  }
  if (verbose) cat('partition size', n, '\n')
  return(S)
}

# Determine n such that |S(2n)-S(n)|<10^-8
simpson(f,0,2, tol=1e-8, verbose=TRUE)

## NEW FUNCTION
f <- function(x){
  sin(1/x)
}

a=1/(5*pi)
b=1
#png(file="Integration_F2.png", width=720, height=480)
curve(f,a,b,col=4,lwd=2,main="sin(1/x)",n=1000)
abline(0,0)
x<-seq(a,b,length=9)
points(x,rep(0,9),cex=1,col=3,pch=16)
x<-seq(a,b,length=5)
points(x,rep(0,5),cex=1,col=2,pch=16)
x<-seq(a,b,length=3)
points(x,rep(0,3),cex=1,col=1,pch=16)
#dev.off()

quadrature <- function(ftn, a, b, tol = 1e-8, trace = FALSE) {
  # numerical integral of ftn from a to b
  # ftn is a function of one variable
  # the partition used is recursively refined until the
  # estimate on successive partitions differs by at most tol
  # if trace is TRUE then intermediate results are printed
  #
  # the main purpose of this function is to call function q.recursion
  #
  # the function returns a vector of length 2 whose first element
  # is the integral and whose second element is the number of
  # function evaluations required
  
  c = (a + b)/2
  fa <- ftn(a)
  fb <- ftn(b)
  fc <- ftn(c)
  h <- (b - a)/2
  I.start <- h*(fa + 4*fc + fb)/3 # Simpson's rule
  q.out <- q.recursion(ftn,a,b,c,fa,fb,fc,I.start,tol,1,trace)
  q.out[2] <- q.out[2] + 3
  if (trace) {
    cat("final value is", q.out[1], "in",
        q.out[2], "function evaluations\n")
  }
  return(q.out)
}

q.recursion <- function(ftn,a,b,c,fa,fb,fc,I.old,tol,level,trace) {
  # refinement of the numerical integral of ftn from a to b
  # ftn is a function of one variable
  # the current partition is [a, c, b]
  # fi == ftn(i)
  # I.old is the value of the integral I using the current partition
  # if trace is TRUE then intermediate results are printed
  # level is the current level of refinement/nesting
  #
  # the function returns a vector of length 2 whose first element
  # is the integral and whose second element is the number of
  # function evaluations required
  #
  # I.left and I.right are estimates of I over [a, c] and [c, b]
  # if |I.old - I.left - I.right| <= tol then we are done, otherwise 
  # I.left and I.right are recursively refined
  
  level.max <- 64
  if (level > level.max) {
    cat("recursion limit reached: singularity likely\n")
    return(NULL)
  } else {
    h <- (b - a)/4
    f1 <- ftn(a + h)
    f2 <- ftn(b - h)
    I.left <- h*(fa + 4*f1 + fc)/3  # Simpson's rule for left half
    I.right <- h*(fc + 4*f2 + fb)/3 # Simpson's rule for right half
    I.new <- I.left + I.right       # new estimate for the integral
    f.count <- 2
    
    if (abs(I.new - I.old) > tol) { # I.new not accurate enough
      q.left <- q.recursion(ftn, a, c, a + h, fa, fc, f1, I.left,
                            tol/2, level + 1, trace)
      q.right <- q.recursion(ftn, c, b, b - h, fc, fb, f2, I.right,
                             tol/2, level + 1, trace)
      I.new <- q.left[1] + q.right[1]
      f.count <-  f.count + q.left[2] + q.right[2];
    } else { # we have achieved the desired tolerance
      if (trace) {
        cat("integral over [", a, ", ", b, "] is ", I.new,
            " (at level ", level, ")\n", sep = "")
      }
    }
    
    return(c(I.new, f.count))
  }
}

# Demo of Adaptive Simpson
quadrature(f,a,b,tol=0.02,trace=TRUE)
quadrature(f,a,b)
# Built-in integrate function
integrate(f,a,b)
