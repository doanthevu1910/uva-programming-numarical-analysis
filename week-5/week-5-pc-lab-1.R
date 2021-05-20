setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 5")

rm(list = ls())

#1
x <- 0:3
y <- x^2+3*x+2

library("pracma")

p <- polyfit(x[1:2], y[1:2], 1); p
polyval(p, 1/2) 

fy <- function(x) {
  x^2+3*x+2
}

error <- NULL

for (i in 0:2) {
  p <- polyfit(x[(i+1):(i+2)], y[(i+1):(i+2)], 1)
  pvalue <- polyval(p, i+0.5)
  error[i+1] <- fy(i+0.5) - pvalue
  cat(sprintf("x=%.3f y(true)=%6.3f y(approx)=%6.3f error=%6.3f", i+0.5, fy(i+0.5), pvalue, error[i+1])); cat("\n")
}

#2

x <- c(0, pi/2, pi)
f <- c(0, 1, 0)

df <- c(f[2]-f[1], f[3]-f[2])

d2f <- c(df[2]-df[1])

#OR 
df <- diff(f)
d2f <- diff(df)

cat("x      f      df       d2f", "\n") 
cat("==========================", "\n")
cat(sprintf("%9.6f %8.6f %8.6f %8.6f", x[1], f[1], df[1], d2f[1]), "\n")
cat(sprintf("%9.6f %8.6f %8.6f", x[2], f[2], df[2]), "\n")
cat(sprintf("%8.6f %8.6f", x[3], f[3]), "\n")

polyfit(x[1:3], y[1:3], 2)

#3

x <- c(-pi/2, 0, pi/2, pi)
y <- c(-1,0,1,0)

p <- polyfit(x, y, 3)

y1 <- polyval(p,x)

plot(x,y)
lines(x, y1, col = "red")

#4

?cubicspline

x <- c(-pi, -pi/2, 0, pi/2, pi)
y <- c(0, -1, 0, 1, 0)

xs <- seq(-pi, pi, by = pi/20)

pp <- cubicspline(x, y)
y1 <- ppval(pp, xs)

plot(x, y, ylim=c(-1.5, 1.5), xlim = c(-4,4))
lines(spline(x,y), col="blue")

x<-c(-pi, -pi/2, 0, pi/2, pi)
y<-c(0, -1, 0, 1, 0)

plot(cubicspline(x, y, xi = NULL, endp2nd = FALSE, der = c(0, 0)))

a <- cubicspline(x, y, xi = NULL, endp2nd = FALSE, der = c(0, 0))

y1 <- ppval(a, xs)

y1


#5
data <- read.table(file = "YX.txt",header=TRUE)

View(data); attach(data)

SSR <- function(coef, y, x) {
  fit <- coef[1]*sin(x)+coef[2]*cos(x)
  residuals <- y-fit
  return(sum(residuals^2))
}

results <- optim(c(0,0),SSR,gr=NULL,y,x,method = c("BFGS"))
coef <- results$par
cat(sprintf("a=%10.4f b=%10.4f\n",coef[1],coef[2]))

fit_ols <- coef[1]*sin(x)+coef[2]*cos(x)
par(cex=1.25)
plot(x,fit_ols,type="l",lty=1,lwd=4,col="red")
points(x,y,lwd=2)

detach(data)

#6

