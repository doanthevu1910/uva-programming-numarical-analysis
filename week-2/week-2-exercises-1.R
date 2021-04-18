getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 2")

rm(list = ls())

#5.1
a <- list(seq(from=-4,to=4,length=20),matrix(c(F,T,T,T,F,T,T,F,F),nrow=3,ncol=3),c("don","quixote"),factor(x=c("LOW","
MED","LOW","MED","MED","HIGH")))

a[[2]][c(2,1), c(2,3)]

a[[3]][1] <- sub(pattern="d",replacement="D",x=a[[3]][1])
a[[3]][2] <- sub(pattern="q",replacement="Q",x=a[[3]][2])

cat("\"Windmills! ATTACK!\"\n\t-\\",a[[3]][1]," ",a[[3]][2],"/-",sep="")

a[[1]][a[[1]]>1]

which(a[[4]]=="MED")

b <-  list(facs=a[[4]],nums=c(3,2.1,3.3,4,1.5,4.9),oldlist=a[1:3])

b$facs[b$nums >= 3]

b$flags <- rep(x=b$oldlist[[2]][,3],times=2)

b$nums[!b$flags]

b$oldlist[[3]] <- "Don Quixote"

#5.2
dframe <- data.frame(person=c("Stan","Francine","Steve","Roger","Hayley","Klaus"),sex=factor(x=c("M","F","M","M","F","M")),funny=factor(x=c("High","Med","Low","High","Med","Med"),levels=c("Low","Med","High")),stringsAsFactors=T)

dframe

dframe$age <- c(41,41,15,1600,21,60)

dframe <- dframe[,c(1,4,2,3)]

dframe2 <- data.frame(person=c("Stan","Francine","Steve","Roger","Hayley","Klaus"),sex=factor(x=c("M","F","M","M","F","M")),funny=factor(x=c("High","Med","Low","High","Med","Med"),levels=c("Low","Med","High")),stringsAsFactors=T)

mydframe <- cbind(dframe,dframe2)
mydframe

dframe[substr(x=dframe$person,start=1,stop=1)=="S",]

#6.1
foo <- c(13563,-14156,-14319,16981,12921,11979,9568,8833,-12968,8133)

foo[is.finite(foo^75)]

foo[-which(foo^75==-Inf)]

bar <- matrix(c(77875.4,-35466.25,-39803.81,27551.45,-73333.85,55976.34,23764.3,36599.69,76694.82,-36478.88,-70585.69,47032),nrow=3,ncol=4)
bar

which(is.nan(bar^65/Inf),arr.ind=T)

bar[!is.nan(bar^67+Inf)] == bar[bar^67!=-Inf]

bar[bar^67==-Inf|is.finite(bar^67)]

#6.2
foo <- c(4.3,2.2,NULL,2.4,NaN,3.3,3.1,NULL,3.4,NA)

length(foo) == 8

which(x=is.na(foo))

is.null(x=foo)

is.na(foo[8]) + 4/NULL

b <- list(c(7,7,NA,3,NA,1,1,5,NA))

names(b) <- "alpha"

b

is.null(b$beta)

b$beta <- which(is.na(b$alpha))

b

#6.3
foo <- array(data=1:36,dim=c(3,3,4))
bar <- as.vector(foo)
baz <- as.character(bar)
qux <- as.factor(baz)
quux <- bar+c(-0.1,0.1)

class(foo)

foo.sum <- is.numeric(foo)+is.integer(foo)
bar.sum <- is.numeric(bar)+is.integer(bar)
baz.sum <- is.numeric(baz)+is.integer(baz)
qux.sum <- is.numeric(qux)+is.integer(qux)
quux.sum <- is.numeric(quux)+is.integer(quux)
myfac <- factor(x=c(foo.sum,bar.sum,baz.sum,qux.sum,quux.sum),levels=c(0,1,2))
myfac
as.numeric(myfac)

foo <- matrix(data=2:13,nrow=3,ncol=4)
as.character(as.vector(t(foo)))

foo <- cbind(c(34,23,33,42,41),c(0,1,1,0,0),c(1,2,1,1,2))

foo <- as.data.frame(foo)

foo[,2] <- as.logical(foo[,2])

