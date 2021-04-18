getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 3")

rm(list = ls())

#11.1

myfib4 <- function(thresh,printme){
  if(printme){
    fib.a <- 1
    fib.b <- 1
    cat(fib.a,", ",fib.b,", ",sep="")
    repeat{
      temp <- fib.a+fib.b
      fib.a <- fib.b
      fib.b <- temp
      cat(fib.b,", ",sep="")
      if(fib.b>thresh){
        cat("BREAK NOW...")
        break
      }
    }
  } else {
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
}
myfib4(thresh = 150, printme = TRUE)

myfib4(1000000,T)

myfac <- function(int){
  if(int<0){
    return(NaN)
  }
  result <- 1
  while(int>1){
    result <- result*int
    int <- int-1
  }
  return(result)
}

myfac(-5)
myfac(12)

#11.2

comp <- function(P,i,t=12,y,plotit=TRUE,...){
  yseq <- 1:y
  values <- P*(1+i/(100*t))^(t*yseq)
  
  if(plotit){
    plot(yseq,values,type="s",...)
  } else {
    return(values)
  }
}

comp(5000, 4.4, y=10, plotit = T)[10]

quad <- function(k1,k2,k3){
  if(any(c(missing(k1),missing(k2),missing(k3)))){
    return("At least one of k1, k2, k3 was missing")
  }
  x <- k2^2-4*k1*k3
  if(x<0){
    cat("No real roots\n")
  } else if(x==0){
    return(-k2/(2*k1))
  } else {
    return(c((-k2-x^0.5)/(2*k1),(-k2+x^0.5)/(2*k1)))
  }
}

quad(2,-1)

#11.3

facrec <- function(x){
  if(x==0){
    return(1)
  } else {
    return(x*facrec(x-1))
  }
}

geolist <- function(x){
  geo <- function(nums){
    return(prod(nums)^(1/length(nums)))
  }
  
  for(i in 1:length(x)){
    if(!is.matrix(x[[i]])){
      x[[i]] <- geo(x[[i]])
    } else {
      x[[i]] <- apply(x[[i]],1,geo)
    }
  }
  return(x)
}

#12.1

#12.2