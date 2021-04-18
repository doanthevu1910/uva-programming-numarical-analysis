getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 1")

rm(list = ls())

library(stats)

df <- scale(mtcars)

heatmap(df, scale = "none")

a <- 2

a <- a + 1

print(a)

typeof(a)

class(a)

length(a)

attributes(a)

attributes(matrix(0,2,2))

#basic data types
##character
##numeric - integer or double
##integer 2L
##logical TRUE, FALSE
##complex: 1 + 4i

#data structures
##matrix
##list
##data frame
##factors

(1+4i)^2

.Machine$integer.max; as.integer(2147483648)

1 - 1e-16 == 1

1 - 1e-17 == 1

## R cannot distinguish between too big/too small numbers?

2*(3-1)^2

typeof(2L*3L)

typeof(2L*3)

#booleans
1<=2<4

1<=2 & 2<4

!(1<2)

#strings - texts

s1 <- "R"

s2 <- "is easy"

line <- paste0(s1," ",s2)

line

nchar(line)

substr(line, 3, 3)

#vectors - only row vectors

x <- c(0,1,1,2,3,5,8,13,21)

length(x)

x[2:4]

seq(1,11,2)

1:10 #colon shortcut

seq(1,3, l=5)

x1 <- seq(5); x2 <- seq(2); x1 + x2

#lists

list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,-1,2,8), nrow = 2), list("green",12.3))

print(list_data)

names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")

list_data$A_Matrix

list_data$`A Inner list`[[2]]

#conditional branch - if else statement 
x <- as.integer(readline("Enter a number:"))

if(x<0){
  print("A")
} else if(x>9) {
  print("B")
} else {
  print("C")
}

#while loops

x <- 1 #set this to -1 to run

while ((x<0) | (x>9)) {
  x <- as.integer(readline("enter a number"))
  if(x<0) print("A")
  if(x>9) print("B")
}

cat("C",x)

#for loop

squares = vector()

for(i in 1:100) {
  squares[i] <- i^2
}

print(squares)

n <- 7 
f <- 1

for (i in 1:n) {
  f <- f*i
}

#functions

mypower <- function(x, y) {
  return(x^y)
}

mypower(2,3)

mypower(y = 3, x = 2)

#examples

plusminus <- function(a, b){
  return(list(a+b, a-b))
}
x <- plusminus(1, 2); cat(x[[1]], x[[2]])

plusminus <- function(a, b){
  return(list(plus = a+b, minus = a-b))
}
x <- plusminus(1, 2); cat(x[[1]], x[[2]])

#functions can specify default arguments 
mypower <- function(x, y=2){
  return(x^y)
}

#variable scope

f <- function(){
  z <- 1
  cat("In f, z equals", z)
}
f()

z

#calling convention

x <- 1
f <- function(y){
  y<-2
  cat("In f, y equals", y)
}

cat("Before f(x): x=", x)

f(x)

#anonymous functions

#tomorrow PC labs

