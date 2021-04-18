getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 1")

rm(list = ls())

args(matrix)

mat <- matrix(data = 1:9, nrow = 3, byrow = TRUE)
mat

mat2 <- matrix(1:9, 3)
mat2

row_names <- c("A", "B", "C")
col_names <- c("I", "II", "III")

mat <- matrix(1:9, 3, dimnames = list(row_names, col_names))
mat

dim(mat)

mat <- matrix(1:12, nrow = 3)
dim(mat)

dim(mat) <- c(4,3)
mat

vec <- sample(10)
vec

dim(vec) <- c(2,5)
mat <- matrix(vec, nrow = 2); mat

vec <- sample(4)
as.matrix(vec)

vec2 <- c("A", "B", "c")
as.matrix(vec2)

A <- matrix(sample(9), nrow = 3); A

B <- matrix(1:9, nrow = 3, byrow = TRUE); B

A+B

A+5

A/B

A/B[,1]

A*B

A%*%B

solve(A)

A%*%solve(A)

x <- matrix(1:3, nrow = 1)

rbind(A, x)

cbind(A, t(x))

A[2,2]

A[2,]

A[,2]

A[,]

A[c(3,2,1), c(2,3,1)]

A[1:2,]

A[seq(3,1,-1), c(1,3)]

A[A>4]

#MAGICAL MATRICES

library(magic)

A <- magic(3); A

colSums(A)

rowSums(A)

diag(A)

sum(diag(A))

A[c(3,2,1), 1:3]

sum(diag(A[c(3,2,1), 1:3]))

#ALGORITHMS 

#MODIFIED AMRMSTRONG NUMBER

Armstrong <- function(x){
  if(x%%10==0) return(FALSE)
  else {
    tmp <- x
    count <- 0
    while (tmp>0) {
      count <- count + 1
      tmp <- tmp%%10
    }
    tmp <- x
    sum <- 0
    while (tmp>0) {
    sum <- sum + (tmp%%10)^count
    tmp <- tmp%%10
    }
    if (som==x) return(TRUE)
    else return(FALSE)
  }
}

Armstrong(153)

