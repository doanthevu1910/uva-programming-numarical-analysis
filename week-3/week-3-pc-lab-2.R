getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 3")

rm(list = ls())

#1

geometric <- function(r,n) {
  print((1-r^(n+1))/(1-r))
}

geometric(1.03,10)

#OR

geometric <- function(r,n) {
  v <- 0:n
  terms <- r^v
  return(sum(terms))
}

#2

Euler <- function(n){
  i <- 1
  x <- NULL
  while(i<=n) {
    x <- c(x, 1/(i^2))
    i <- i+1
  }
  return(sum(x))
}

#OR

Euler <- function(n){
  i <- 1:n
  return(sum((1/i)^2))
}

n <- as.numeric(readline("Enter value for n: "))

cat("pi^2/6 is 1.6449\n")
cat((sprintf("The sum of the first %s terms is: %.4f\n",n, Euler(n))))

#3

set.seed(321)
x<-matrix(sample(1:30,3*5,replace=T),3,5); x

maxmat <- function(mat){
  maxcol <- mat[1,]
  maxrow <- mat[,1]
  maxtot <- mat[1,1]
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if(mat[i,j]>maxtot) {maxtot <- mat[i,j]}
      if(mat[i,j]>maxcol[j]) {maxcol[j] <- mat[i,j]}
      if(mat[i,j]>maxrow[i]) {maxrow[i] <- mat[i,j]}
      
    }
  }
  
  results <- list(maxcol, maxrow, maxtot)
  return(results)
}

maxmat(x)
x

#4

dist <- function(x1,y1,x2,y2) {
  return(sqrt((x1-x2)^2+(y1-y2)^2))
}

areatri <- function(x1,y1,x2,y2,x3,y3) {
  a <- dist(x1,y1,x2,y2)
  b <- dist(x2,y2,x3,y3)
  c <- dist(x3,y3,x1,y1)
  s <- (a+b+c)/2
  area <- sqrt(s*(s-a)*(s-b)*(s-c))
  return(area)
}

areatri(0,0,4,0,4,3)

#5

x<- -1:1; M <- 4

approx <- function(x, M) {
  a <- NULL
  for(i in 1:M) {
    a <- c(a, (x^(2*i-1))/(2*i-1))
  }
  a
  sum(a)
  return(2*sum(a))
}

approx(-1, 4); approx(1,4)

Approx <- function(x, M) {
  re <- NULL
  for (i in 1:length(x)) {
    re <- c(re, approx(x[i],M))
  }
  return(re)
}

#OR

Approx <- function(x,M){
  taylor <- rep(0,length(x))
  for(n in 1:M){
    taylor=taylor+2*x^(2*n-1)/(2*n-1);
  }
  return(taylor)
}

M <- 4
x <- seq(-0.99, 0.99, 0.03)
f <- log((1+x)/(1-x))
g <- Approx(x,M);g

library(ggplot2)

ggplot(mapping = aes(x=x)) +
  geom_line(aes(y=f),size=1,col="blue") +
  geom_point(aes(y=g),shape=8,size=2,col="red") +
  xlab("x") +  ylab("f(x) = solid blue line") + 
  labs(title = "Function and its Taylor approximation")

cat(' x log((1+x)/(1-x)) Taylor\n')
cat('--------------------------------------\n')

for(i in 1:5){
  a <- x[i];
  cat(sprintf('%.2f %.6f %.6f\n', a, log((1+a)/(1-a)), g[i]) )
}

cat("log((1+x)/(1-x))~2(");
for(n in 1:M){
  cat(sprintf("x^%d/%d",(2*n-1),(2*n-1)))
  if(n<M)
    cat('+')
  else
    cat(")\n")
}

data <- data.frame(x=x, f=f, g=g)

ggplot() + 
  geom_line(aes(x=x, y=f),size=1,col="blue") + 
  geom_point(aes(x=x, y=g),shape=8,size=2,col="red") +
  xlab("x") +  ylab("f(x) = solid blue line") + 
  labs(title = "Function and its Taylor approximation")