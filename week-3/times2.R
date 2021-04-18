times2 <- function(x){
  k <- length(x)
  x2 <- rep(0,k)
  carry <- 0
  i <- k
  
  while(i>=1) {
    tmp <- 2*x[i] + carry
    carry <- 0
    
    if(tmp > 9) {
      carry <- 1
      x2[i] <- (tmp-10)
      i <- (i-1)
    }
    
    else {x2[i] <- tmp
    i <- (i-1)
    }
  }
  
  if(carry>0){
    x2 <- c(1,x2)
    return(x2)
  }
  
  return(x2)
}

