getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 1")

rm(list = ls())

#3

m <- 1000000L
len <- c(1,2,8,3,6, rep(0L, m-5))
for(n in 6:m){ 
  x <- n 
  count <- 0 
  while (x >= n) { 
    count <- count + 1 
    if(x %% 2 == 0) { x<- x %/% 2 
    } else {
      x <- 3*x + 1 
    } 
    if (x == n) { 
      stop(paste("Starting value",n,"leads to a cycle!",sep=" "), call.=FALSE) } 
  } 
  len[n] <- count+len[x] 
}

steps <- 0
n_start <- 0

for (n in 1:m) {
  if(len[n]>steps){
    steps <- len[n]
    n_start <- n
  }
}

cat("Starting value ",n_start," takes ",steps," steps.") 

#4

sprintf("%s %f", "test", 1:3)

x=matrix(c(1,0,0,0,1,1,0,1,0,0,0,0,0,1,1,0,1,0,1,0,0,0,1,0,1),5,5); x

Neighbours<-function(x,i,j){ 
  living<-0 
  if( (i-1)>0 & (j-1)>0 ) living<-living+x[i-1,j-1] 
  if( (i-1)>0 ) living<-living+x[i-1,j] 
  if( (i-1)>0 & (j+1)<6 ) living<-living+x[i-1,j+1] 
  if( (j-1)>0) living<-living+x[i,j-1] 
  if( (j+1)<6) living<-living+x[i,j+1] 
  if( (i+1)<6 & (j-1)>0 ) living<-living+x[i+1,j-1] 
  if( (i+1)<6) living<-living+x[i+1,j] 
  if( (i+1)<6 & (j+1)<6 ) living<-living+x[i+1,j+1] 
  return(living) 
}

y <- matrix(0,5,5); y

for (i in 1:5) {
  for (j in 1:5) {
    if(x[i,j]==0) {
      if(Neighbours(x,i,j)==3) y[i,j] <- 1
    }
    if(x[i,j]==1){
      count <- Neighbours(x,i,j)
      if(count==2|count==3) y[i,j] <-1
    }
  }
}

y

sum(y)

