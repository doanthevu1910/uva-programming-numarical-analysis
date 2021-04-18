getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 2")

rm(list = ls())

#1
"1 x 8 + 1 = 9
12 x 8 + 2 = 98
123 x 8 + 3 = 987
1234 x 8 + 4 = 9876
12345 x 8 + 5 = 98765
123456 x 8 + 6 = 987654
1234567 x 8 + 7 = 9876543
12345678 x 8 + 8 = 98765432
123456789 x 8 + 9 = 987654321"

xstr <- ""
i <- 1
ystr <- ""

for(i in c(1:9)) {
  xstr <- paste0(xstr, i)
  ystr <- paste0(ystr, 10-i)
  cat(sprintf("%s x 8 + %d = %s\n", xstr, i, ystr))
}

#2
actual <- exp(-1)
diff <- 1
n <- 0 
while(diff >= 0.0001){
  n <- n+1
  approx <- (1-1/n)^n;
  diff <- abs(actual-approx);
}

actual
n

cat(sprintf('The value for n is %d\n',n))

#3
install.packages("tictoc")
library(tictoc)
tic()
k<-1000
found<-FALSE
a<-0
maxa<-k
while( (a<maxa) & !found ){
  a<-a+1
  b<-0
  maxb<-k
  while( (b<maxb) & !found ){
    b<-b+1
    c<-0
    maxc<-k
    while( (c<maxc) & !found ){
      c<-c+1
      if( (a+b+c==k) & (a^2+b^2==c^2) ) {
        found<-TRUE
      }
    }
  }
}
cat("Solution:",a*b*c,"\n")
toc()

tic()
found<-FALSE
a<-0
maxa<-k %/%3
while( (a<maxa) & !found ){
  a<-a+1
  b<-a
  maxb<-(k-a) %/% 2
  while( (b<maxb) & !found ){
    b<-b+1
    c<-k-a-b
    if( (a+b+c==k) & (a^2+b^2==c^2) ) {
      found<-TRUE
    }
  }
}
cat("Solution:",a*b*c,"\n")
toc()

tic()
for( m in 501:1000 ){
  if( 500000 %% m==0){
    a<-1000-(500000 %/% m)
    b<-1000-m
    c<-1000-a-b
    break
  }
}
cat("Solution:",a*b*c,"\n")
toc()


