b<-as.numeric(readline("Give a value for b:"))

f <- function(x){
  exp(2/x)
}

g <- function(x,b) {
  exp(2/b)-2*(x-b)/(b^2)*exp(2/b)+2*(1+b)*((x-b)^2)/(b^4)*exp(2/b)
}

x <- seq(2,4,0.1)

cat("Values for f(x)\n")
print(f(x))
cat("Values for g(x)\n")
print(g(x,b))

table <- cbind(x, f(x), g(x,b))
colnames(table) <- c("x","f(x)", "g(x)")
table

table <- as.data.frame(table)

ggplot(table)

tablef <- data.frame(cbind(table[,1], table[,2]))

ggplot(tablef)

