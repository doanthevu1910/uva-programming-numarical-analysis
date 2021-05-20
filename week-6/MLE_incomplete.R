logLik<-function(theta,x) {
  # CODE THE LOG-LIKELIHOOD HERE
}

gr_logLik<-function(theta,x) {
  # CODE THE SCORE HERE
}

x <- scan("countdata.dat")
n <- length(x)

cat('Estimated parameters\n')
result1 <- optim(4,logLik,gr=NULL,x,method="Brent",
                 lower=0,upper=10,control=list(fnscale=-1))
print(result1$par)
result2 <- optim(4,logLik,x=x,gr_logLik, method="BFGS",
                 control=list(fnscale=-1,maxit=300,trace=TRUE,REPORT=5))
names(result2)  # all properties of the object result2
print(result2$par)