getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Past Exams")

rm(list = ls())

sequence <- function(n) {
  s <- c(2,3,4)
  i <- 4
  while (i <= n) {
    s[i] <- s[i-2] + s[i-3]
    i <- i+1
  }
  return(s)
}

sequence(5)

seq <- sequence(16)

a <- NULL
for (i in 2:16) {
  a[i-1] <- seq[i]/seq[i-1]
}
a
#OR
ratio <- seq[2:16]/seq[1:15]; ratio

for (i in 11:16) {
  cat(sprintf("%7d", i))
}
cat(sprintf("\n"))
for (i in 11:16) {
  cat(sprintf("%7.3f", ratio[i-1]))
}
#OR
cat(sprintf("%7d",11:16)); cat(sprintf("\n"))
cat(sprintf("%7.3f",ratio[10:15])); cat(sprintf("\n"))

plot(x = 2:16, y = ratio, pch=3, col = "magenta", xlab = "i", ylab = "Ratio", main = "S[i]/S[i-1]")

q <- matrix(c(0,1,0,0,0,1,1,1,0), 3, 3, byrow = TRUE); q

seqlist <- list(matrix(c(2,3,4),3,1))

x <- list(c(2,3,4)); q%*%x[[1]]

for (i in 2:14) {
  seqlist[[i]] <- q%*%seqlist[[i-1]]
}

counter <- 0
for (i in 1:length(seqlist)) {
  x <- seqlist[[i]]
  if (x[3]==seq[i+2]) counter <- counter +1
}

cat(sprintf("Counter = %d\n", counter))