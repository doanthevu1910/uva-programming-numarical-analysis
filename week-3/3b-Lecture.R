## 
mysquare <- function(x){
  return(x^2)
}
ls()
mysquare(3)


##
myfib3 <- function(thresh){
  fibseq <- c(1,1)
  counter <- 2
  repeat{
    fibseq <- c(fibseq,fibseq[counter-1]+fibseq[counter])
    counter <- counter+1
    if(fibseq[counter]>thresh){
      break
    }
  }
  return(fibseq)
}
myfib3(1e3)


##
dummy1 <- function(){ a <- 1; b <- 2}
dummy1()
c <- dummy1()
cat(c)
dummy3 <- function(){ a <- 1; b <- 2; 
                      return(a)}
dummy3()


##
f <- function(a, b= 10){  
  return(a+b)
}

f(10)
f(10,5)
f(b=5,a=10)


##
mysum <- function(x,y,...) {
  args=list(...)
  som <- x+y
  if(length(args)>0)
     for(z in args){
       som <- som+z
     }
  return(som)
}
mysum(1,2)
mysum(1,2,3,4)
mysum(1,2,3:4)


##
roots <- function(a,b,c){
  discriminant<-function(a,b,c){
    b^2-4*a*c
  }
  D=discriminant(a,b,c)
  if(D > 0){ # first case D>0
    x_1 = (-b+sqrt(D))/(2*a)
    x_2 = (-b-sqrt(D))/(2*a)
    return(c(x_1,x_2))
  }
  else if(delta(a,b,c) == 0){ # second case D=0
    return(-b/(2*a))
  }
  else {return("There are no real roots.")} # third case D<0
}

roots(1,-6,8)


##
g <- function(x){
  return(x^2)
}

integrate(g,0,2)
integrate(function(x){x^2},0,2)


##
f <- function(x,mu,sigma){
  z <- (x - mu)/sigma
  return( 1/sqrt(2*pi*sigma)*exp(-1/2*z^2) )
}


##
integrate(f,-Inf,0)
integrate(function(x) f(x,0,1),-Inf,0)


## 
df <- data.frame(first=5:7, second=(0:2)^2, third=-1:1)
df
apply(df, 2, function(x) { sqrt(sum(x^2)) })


## 
set.seed(1)
x <- sample(1:6, 12, replace=TRUE)
mat <- matrix(x, nrow=3)
mat
apply(mat, 1, function(x) { seq(min(x), max(x)) })


##
facn <- function(n) {
  if(n == 1) {
    return(1)
  } else { 
    return(n * facn(n-1))
  }
}
facn(3)


##
facn <- function(n) {
  if(n == 1) {
    return(1)
  } else { 
    return(n * facn(n-1))
  }
}
facn(3)


##
descr.stats <- function(x){
  return(list(xbar=mean(x),std=sd(x)))
}

data <- rnorm(100)
stats <- descr.stats(data)
stats
print(coefficient.of.variation <- stats$std/stats$xbar)
