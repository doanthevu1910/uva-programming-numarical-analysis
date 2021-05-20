rm(list = ls()) # clear memory
cat("\f")       # clear screen
# 3x+2y=18 -> y=9-1.5x
# -x+2y=2  -> y=1+0.5x
A <- matrix(c( 3,2,
              -1,2)
            ,2,2,byrow=T)
b <- c(18,2)
solve(A,b)

# two plots side-by-side
par(mfrow=c(2,1))
# first plot
curve(9-1.5*x,0,6,col=4,lwd=2)
abline(a=1,b=0.5,col=2,lwd=2)
points(x=4,y=3,type="p",col=3,pch=16,cex=2)
# small change in the x-coefficient first equation
A <- matrix(c( 3.01,2,
                 -1,2)
            ,2,2,byrow=T)
solve(A,b)
# second plot
curve(9-3.01/2*x,0,6,col=4,lwd=2)
abline(a=1,b=0.5,col=2,lwd=2)
points(x=3.99,y=2.995,type="p",col=3,pch=16,cex=2)

cat("\f")   # clear screen
# -0.48x+y=1.1 -> y=1.1+0.48x
# -0.50x+y=1.0 -> y=1.0+0.50x
A <- matrix(c( -0.48,1,
               -0.50,1)
            ,2,2,byrow=T)
b <- c(1.1,1.0)
solve(A,b)
# two plots side-by-side
par(mfrow=c(2,1))
# first plot
curve(1.1+0.48*x,0,12,col=4,lwd=2)
abline(a=1,b=0.5,col=2,lwd=2)
points(x=5.0,y=3.5,type="p",col=3,pch=16,cex=2)

# small change in the x-coefficient first equation
A <- matrix(c( -0.49,1,
               -0.50,1)
            ,2,2,byrow=T)
b <- c(1.1,1.0)
solve(A,b)
# second plot
curve(1.1+0.49*x,0,12,col=4,lwd=2)
abline(a=1,b=0.5,col=2,lwd=2)
points(x=10,y=6,type="p",col=3,pch=16,cex=2)
# determinant is small: ill-conditioned
det(A)

cat("\f")   # clear screen
# -1/2x+y=1   -> y=1.0+0.5x
# -1/2x+y=1/2 -> y=0.5+0.50x
A <- matrix(c( -0.5,1,
               -0.5,1)
            ,2,2,byrow=T)
b <- c(1,0.5)
solve(A,b)
# plot
par(mfrow=c(1,1))
curve(1.0+0.50*x,0,6,col=4,lwd=2)
abline(a=0.5,b=0.5,col=2,lwd=2)

# Fibonacci Numbers
cat("\f")       # clear screen
A <- matrix(c(1,1,1,0),2,2)
ev <- eigen(A)
str(ev)
# V contains the matrix with normalized eigen vectors
V <- ev$vectors; V
# lambda contains the vector with eigen values
lambda <- ev$values; lambda

l1 <- (1+sqrt(5))/2
l1
v1 <- matrix(c(l1,1),2,1)
v1 <- v1/norm(v1,"F")
v1

F<-c(0,1)
for(i in 1:8){
  F<-c(F,sum(tail(F,2)))
}
F
k<-9
(lambda[1]^k-lambda[2]^k)/sqrt(5)
lambda[1]^k/sqrt(5)

# Markov Chain
cat("\f")    # clear screen
p <- 0.136   # probability to find a job
q <- 0.998   # probability to stay in a job
A <- matrix(c(q,p,1-q,1-p),2,2,byrow=TRUE)
ev <- eigen(A)
# Eigenvector and eigenvalues
lambda <- ev$values; lambda
V <- ev$vectors; V
# Realationship steady state and eigenvector for lambda=1
steady <- matrix(c(p/(1-q+p),1-x),2,1)
steady
steady/norm(steady,"F")
# Initialize for 100 periods
v=matrix(0,2,100)
# Starting state
v0=c(0.95,  # starting with 95% employed
     0.05   # 5% is unemployed
         )
# Determining next 100 states
v[,1]=A %*% v0
for(i in 2:100){
  v[,i]=A %*% v[,i-1]
}
# What is the value after 100 periods
v[,100]
# More efficient way using decomposition
V %*% diag(lambda^100) %*% solve(V) %*% v[,1]
# Plot of the unemployment rate
plot(v[2,])
#

