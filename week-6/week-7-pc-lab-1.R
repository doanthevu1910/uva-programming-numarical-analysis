setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 7")

rm(list = ls())

#1
A <- matrix(0, nrow = 4, ncol = 4); A

r <- 1:4

A[1, ] <- r; A[ , 4] <- rev(r)

#2

A <- matrix(1,3,3); A
B <- 2*matrix(1,3,2); B
C <- 3*matrix(1,2,3); C

cbind(A,B)
cbind(A,t(B))
cbind(A,C)
cbind(A,t(C))

rbind(A,C)
rbind(A,t(B))

#3

A <- matrix(c(1,2,3,4), 2, 2, byrow = TRUE); A
B <- matrix(c(3,4,-1,2), 2, 2, byrow = TRUE); B
A%*%B

A <- matrix(c(3,5,6,-2),2,2, byrow = TRUE); A
B <- matrix(c(-1,0,2,1),2,2,byrow = TRUE); B
2*A - 4*B

A <- c(1,3,5)
B <- matrix(c(2,-1,-1,0,7,-2), 3, 2, byrow = TRUE); B
A%*%B

#4

A <- matrix(c(1,1,2,1,-1,-3,-2,-5,1), 3, 3, byrow = TRUE); A

b <- c(1,0,4)

solve(A, b)

#5 

#6

library(Ryacas)
yac("A:={{0,1,s},{s,0,1},{1,s,0}}")
yac("Determinant(A)")

a <- NULL
for(i in 1:4) {
  s <- (i-1)*(pi/4)
  a[i] <- s^3 + 1
}
a

i <- 1:4
s <- (i-1)*(pi/4)
y <- s^3 + 1

library(pracma)

a <- polyfit(s,y,3); a

y <- polyval(p=a, x=1:100)

plot(y, type = "l")

#7

A <- matrix(c(1,0,0,-1,0,1,0,0,0,0,1,0,-1,0,0,1), 4,4); A
ev <- eigen(A)
V <- ev$vectors; V
lambda <- ev$values; lambda

av <- vector(mode = "list", length = 4)
lv <- vector(mode = "list", length = 4)
for (i in 1:4) {
  av[[i]] <- A%*%V[,i]
  lv[[i]] <- lambda[i]%*%V[,i]  
}

for (i in 1:4) {
  cat(sprintf("A*v_%d  lambda_%d*v_%d", i,i,i), "\n")
  cat(sprintf("%2.5f %2.5f", av[[i]][1], lv[[i]][1]), "\n")
  cat(sprintf("%2.5f %2.5f", av[[i]][2], lv[[i]][2]), "\n")
  cat(sprintf("%2.5f %2.5f", av[[i]][3], lv[[i]][3]), "\n")
  cat(sprintf("%2.5f %2.5f", av[[i]][4], lv[[i]][4]), "\n")
}

for (i in 1:4) {
  cat(sprintf("A*v_%d  lambda_%d*v_%d", i,i,i), "\n")
  for (j in 1:4) {
    cat(sprintf("%2.5f %2.5f", av[[i]][j], lv[[i]][j]), "\n")
  }
}

#8




