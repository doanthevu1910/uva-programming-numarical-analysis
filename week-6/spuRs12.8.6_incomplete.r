f <- function(x) {
  # bug: if x[2] is too large then exp(x[2]) is Inf and cos(x[2]-exp(x[2])) is NaN
  # just set cos(x[2]-exp(x[2])) to 0 in this case
  s <- sum(x^2)
  y <- -(s-2)*(s-1)*s*(s+1)*(s+2)*(2-sin(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2])))
  if (is.nan(y)) {
    y <- -(s-2)*(s-1)*s*(s+1)*(s+2)*2
  }
  return(y)
}

gradf <- function(x) {
  # bug: if x[2] is too large then exp(x[2]) is Inf and cos(x[2]-exp(x[2])) is NaN
  # just set cos(x[2]-exp(x[2])) and sin(x[2]-exp(x[2])) to 0 in this case
  s <- sum(x^2)
  f1 <- -2*x[1]*((s-1)*s*(s+1)*(s+2) + (s-2)*s*(s+1)*(s+2) + (s-2)*(s-1)*(s+1)*(s+2) +
                   (s-2)*(s-1)*s*(s+2) + (s-2)*(s-1)*s*(s+1))*(2-sin(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2]))) +
    2*x[1]*(s-2)*(s-1)*s*(s+1)*(s+2)*cos(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2]))
  f2 <- -2*x[2]*((s-1)*s*(s+1)*(s+2) + (s-2)*s*(s+1)*(s+2) + (s-2)*(s-1)*(s+1)*(s+2) +
                   (s-2)*(s-1)*s*(s+2) + (s-2)*(s-1)*s*(s+1))*(2-sin(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2]))) -
    2*x[2]*(s-2)*(s-1)*s*(s+1)*(s+2)*cos(x[1]^2-x[2]^2)*cos(x[2]-exp(x[2])) -
    (1 - exp(x[2]))*(s-2)*(s-1)*s*(s+1)*(s+2)*sin(x[1]^2-x[2]^2)*sin(x[2]-exp(x[2]))
  if (is.nan(f1)) {
    f1 <- -2*x[1]*((s-1)*s*(s+1)*(s+2) + (s-2)*s*(s+1)*(s+2) + (s-2)*(s-1)*(s+1)*(s+2) +
                     (s-2)*(s-1)*s*(s+2) + (s-2)*(s-1)*s*(s+1))*2
  }
  if (is.nan(f2)) {
    f2 <- -2*x[2]*((s-1)*s*(s+1)*(s+2) + (s-2)*s*(s+1)*(s+2) + (s-2)*(s-1)*(s+1)*(s+2) +
                     (s-2)*(s-1)*s*(s+2) + (s-2)*(s-1)*s*(s+1))*2
  }
  return(c(f1, f2))
}

# use numerical approx of derivative to check gradf is correct
check.gradf <- function(x, e=1e-8) {
  f1 <- (f(x+c(e,0)) - f(x))/e
  f2 <- (f(x+c(0,e)) - f(x))/e
  return(c(gradf(x) - c(f1, f2)))
}
check.gradf(c(0,0))
check.gradf(c(1,1))
check.gradf(c(1,-1))

x <- seq(-1.5, 1.5, .05)
y <- seq(-1.5, 1.5, .05)
xyz <- data.frame(matrix(0, length(x)*length(y), 3))
names(xyz) <- c('x', 'y', 'z')
n <- 0
for (i in 1:length(x)) {
  for (j in 1:length(y)) {
    n <- n + 1
    xyz[n,] <- c(x[i], y[j], max(-3,f(c(x[i], y[j]))))
  }
  5
}
library(lattice)
print(wireframe(z ~ x*y, data = xyz, scales = list(arrows = FALSE),
                zlab = 'f(x, y)', drape = T))

x <- runif(2, -1.5, 1.5)

n <- 100

for (i in 1:n) {
  x[i,] <- ascent(f, gradf, runif(2, -1.5, 1.5), n.max=500)
  x[i,] <- round(x[i,], digits=4)
}

