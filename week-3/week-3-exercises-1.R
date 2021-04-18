getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 3")

rm(list = ls())

#9.1
a <- ls("package:methods")[1:20]

length(a)

environment(read.table)

any(ls("package:graphics")=="smoothScatter")

#9.2
seq(-4,4,0.2)

#10.1
vec1 <- c(2,1,1,3,2,1,0)
vec2 <- c(3,8,2,2,0,0,0)

if(vec1[1]>=2&&vec2[1]>=2){ cat("Print me!") }

ifelse(vec1+vec2>3,vec1*vec2,vec1+vec2)

mymat <- matrix(as.character(1:16),4,4); mymat

if(any(substr(diag(mymat),1,1)=="g")||any(substr(diag(mymat),1,1)=="G")){
  indexes <- which(substr(diag(mymat),1,1)=="g"|substr(diag(mymat),1,1)=="G")
  diag(mymat)[indexes] <- "HERE"
} else {
  mymat <- diag(nrow(mymat))
}

mymat

#10.2

mynum <- 3
mynum <- 0

if(mynum==1){
  foo <- 12
} else if(mynum==2){
  foo <- 34
} else if(mynum==3){
  foo <- 56
} else if(mynum==4){
  foo <- 78
} else if(mynum==5){
  foo <- NA
} else {
  foo <- NULL
}
foo

if(any(doselevel=="High")){
  if(lowdose>=10){
    lowdose <- 10
  } else {
    lowdose <- lowdose/2
  }
  if(meddose>=26){
    meddose <- 26
  }
  if(highdose<60){
    highdose <- 60
  } else {
    highdose <- highdose*1.5
  }
  dosage <- rep(lowdose,length(doselevel))
  dosage[doselevel=="Med"] <- meddose
  dosage[doselevel=="High"] <- highdose
} else {
  doselevel <- factor(doselevel,levels=c("Low","Med"),labels=c("Small","Large"))
  if(lowdose<15 && meddose<35){
    lowdose <- lowdose*2
    meddose <- meddose+highdose
  }
  dosage <- rep(lowdose,length(doselevel))
  dosage[doselevel=="Large"] <- meddose
}

lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low","High","High","High","Low","Med",
                      "Med"),levels=c("Low","Med","High"))

?switch
mynum <- 3
ifelse(mynum>0,switch(mynum,"one","two","three","four","five","six","seven","eight","nine"),"zero")
mynum <- 0
ifelse(mynum>0,switch(mynum,"one","two","three","four","five","six","seven","eight","nine"),"zero")

#10.3

loopvec1 <- 5:7
loopvec2 <- 9:6
foo <- matrix(NA,length(loopvec1),length(loopvec2))
for(i in 1:length(loopvec1)){
  foo[i,] <- loopvec1[i]*loopvec2
}
foo

mystrings <- c("Peter","Homer","Lois","Stewie","Maggie","Bart")
mynums <- rep(NA,length(mystrings))
for(i in 1:length(mystrings)){
  mynums[i] <- switch(EXPR=mystrings[i],Homer=12,Marge=34,Bart=56,Lisa=78,Maggie=90,NA)
}

for(i in 1:length(mylist)){
  member <- mylist[[i]]
  if(is.matrix(member)){
    counter <- counter+1
  } else if(is.list(member)){
    for(j in 1:length(member)){
      if(is.matrix(member[[j]])){
        counter <- counter+1
      }
    }
  }
}

#10.4

mynum <- 7
mynum.fac <- 1
while(mynum>1){
  mynum.fac <- mynum.fac*mynum
  mynum <- mynum-1
}
mynum.fac

#10.5

foo <- matrix(1:12,4,3)
apply(apply(foo,1,sort,decreasing=TRUE),2,prod)

matlist <- list(matrix(c(T,F,T,T),2,2),matrix(c("a","c","b","z","p","q"),3,2),matrix(1:8,2,4))
matlist
for(i in 1:length(matlist)){
  matlist[[i]] <- t(matlist[[i]])
}
matlist
matlist <- list(matrix(c(T,F,T,T),2,2),matrix(c("a","c","b","z","p","q"),3,2),matrix(1:8,2,4))
matlist
matlist <- lapply(matlist,t)
matlist

qux <- array(96:1,dim=c(4,4,2,3))

qux

qux[,,2,]

apply(qux[,,2,],3,diag)

apply(apply(qux[,4,,],3,dim),1,sum)

?apply

#10.6

