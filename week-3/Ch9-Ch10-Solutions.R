######################################
#### SUGGESTED EXERCISE SOLUTIONS ####
######################################

#########
## 9.1 ##
#########
#(a)
ls("package:methods")[1:20]
length(ls("package:methods"))
#(b)
##(i)
environment(read.table)
##(ii)
environment(data)
##(iii)
environment(matrix)
##(iv)
environment(jpeg)
#(c)
any(ls("package:graphics")=="smoothScatter")

#########
## 9.2 ##
#########
#(a)
seq(-4,4,0.2)
#(b)
##(i)
array(8:1,dim=c(2,2,2)) # MIXED: 'data' positional, 'dim' exact.
##(ii)
rep(1:2,3) # POSITIONAL
##(iii)
seq(from=10,to=8,length=5) # MIXED: 'from' and 'to' exact, 'length.out' partial.
##(iv)
sort(decreasing=T,x=c(2,1,1,2,0.3,3,1.3)) # EXACT
##(v)
which(matrix(c(T,F,T,T),2,2)) # POSITIONAL
##(vi)
which(matrix(c(T,F,T,T),2,2),a=T) # MIXED: 'x', 'data', 'nrow', 'ncol' positional, 'arr.ind' partial
#(c)
# 'pch', 'lwd', 'lty' and 'col' are part of the ellipsis.

##########
## 10.1 ##
##########
#(a)
vec1 <- c(2,1,1,3,2,1,0)
vec2 <- c(3,8,2,2,0,0,0)
##(i)
if((vec1[1]+vec2[2])==10){ cat("Print me!") }  # condition SATISFIED
##(ii)
if(vec1[1]>=2&&vec2[1]>=2){	cat("Print me!") }  # condition SATISFIED
##(iii)
if(all((vec2-vec1)[c(2,6)]<7)){	cat("Print me!") }  # condition NOT SATISFIED
##(iv)
if(!is.na(vec2[3])){ cat("Print me!") }  # condition SATISFIED
#(b)
ifelse(vec1+vec2>3,vec1*vec2,vec1+vec2)
#(c)
if(any(substr(diag(mymat),1,1)=="g")||any(substr(diag(mymat),1,1)=="G")){
  indexes <- which(substr(diag(mymat),1,1)=="g"|substr(diag(mymat),1,1)=="G")
  diag(mymat)[indexes] <- "HERE"
} else {
  mymat <- diag(nrow(mymat))
}
mymat
##(i)
mymat <- matrix(as.character(1:16),4,4)
##(ii)
mymat <- matrix(c("DANDELION","Hyacinthus","Gerbera","MARIGOLD","geranium","ligularia","Pachysandra","SNAPDRAGON","GLADIOLUS"),3,3)
##(iii)
mymat <- matrix(c("GREAT","exercises","right","here"),2,2,byrow=T)

##########
## 10.2 ##
##########
mynum <- 3
mynum <- 0
#(a)
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
#(b)
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
##(i)
lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low","High","High","High","Low","Med","Med"),levels=c("Low","Med","High"))
##(ii)
lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low","Low","Low","Med","Low","Med","Med"),levels=c("Low","Med","High"))
##(iii)
lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low","Med","Med"),levels=c("Low","Med","High"))
##(iv)
lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low","High","High","High","Low","Med","Med"),levels=c("Low","Med","High"))
#(c)
mynum <- 3
ifelse(mynum>0,switch(mynum,"one","two","three","four","five","six","seven","eight","nine"),"zero")
mynum <- 0
ifelse(mynum>0,switch(mynum,"one","two","three","four","five","six","seven","eight","nine"),"zero")

##########
## 10.3 ##
##########
#(a)
loopvec1 <- 5:7
loopvec2 <- 9:6
foo <- matrix(NA,length(loopvec1),length(loopvec2))
for(i in 1:length(loopvec1)){
  foo[i,] <- loopvec1[i]*loopvec2
}
foo
#(b)
mystrings <- c("Peter","Homer","Lois","Stewie","Maggie","Bart")
mynums <- rep(NA,length(mystrings))
for(i in 1:length(mystrings)){
  mynums[i] <- switch(EXPR=mystrings[i],Homer=12,Marge=34,Bart=56,Lisa=78,Maggie=90,NA)
}
#(c)
counter <- 0
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
##(i)
mylist <- list(aa=c(3.4,1),bb=matrix(1:4,2,2),cc=matrix(c(T,T,F,T,F,F),3,2),dd="string here",ee=list(c("hello","you"),matrix(c("hello","there"))),ff=matrix(c("red","green","blue","yellow")))
##(ii)
mylist <- list("tricked you",as.vector(matrix(1:6,3,2)))
##(iii)
mylist <- list(list(1,2,3),list(c(3,2),2),list(c(1,2),matrix(c(1,2))),rbind(1:10,100:91))

##########
## 10.4 ##
##########
#(a)
##(i)
mylist <- list()
counter <- 1
mynumbers <- c(2,2,2,2,5,2)
mycondition <- mynumbers[counter]<=5
mycondition
while(mycondition){
  mylist[[counter]] <- diag(mynumbers[counter])
  counter <- counter+1
  if(counter<=length(mynumbers)){
    mycondition <- mynumbers[counter]<=5
  } else {
    mycondition <- FALSE
  }
}
mylist
##(ii)
mylist <- list()
counter <- 1
mynumbers <- 2:20
mycondition <- mynumbers[counter]<=5
mycondition
while(mycondition){
  mylist[[counter]] <- diag(mynumbers[counter])
  counter <- counter+1
  if(counter<=length(mynumbers)){
    mycondition <- mynumbers[counter]<=5
  } else {
    mycondition <- FALSE
  }
}
mylist
##(iii) below, the loop braced-area code won't even be entered -- the first element of 'mynumbers' is greater than 5 -- resulting list will be empty
mylist <- list()
counter <- 1
mynumbers <- c(10,1,10,1,2)
mycondition <- mynumbers[counter]<=5
mycondition
while(mycondition){
  mylist[[counter]] <- diag(mynumbers[counter])
  counter <- counter+1
  if(counter<=length(mynumbers)){
    mycondition <- mynumbers[counter]<=5
  } else {
    mycondition <- FALSE
  }
}
mylist
#(b)
mynum.fac <- 1
while(mynum>1){
  mynum.fac <- mynum.fac*mynum
  mynum <- mynum-1
}
mynum.fac
##(i)
mynum <- 5
##(ii)
mynum <- 12
##(iii)
mynum <- 0
#(c)
mystring <- "R fever"
mystring <- "beautiful"
mystring <- "ECCENTRIC"
mystring <- "ElAbOrAte"
mystring <- "eeeeek!"
#--#
index <- 1
ecount <- 0
result <- mystring
while(ecount<2 && index<=nchar(mystring)){
  temp.char <- substr(mystring,index,index)	
  if(temp.char=="e"||temp.char=="E"){
    ecount <- ecount+1
  }
  if(ecount==2){
    result <- substr(mystring,1,index-1)
  }
  index <- index + 1
}
result

##########
## 10.5 ##
##########
#(a)
foo <- matrix(1:12,4,3)
apply(apply(foo,1,sort,decreasing=TRUE),2,prod)
#(b)
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
#(c)
qux <- array(96:1,dim=c(4,4,2,3))
##(i)
apply(qux[,,2,],3,diag)
##(ii)
apply(apply(qux[,4,,],3,dim),1,sum)

##########
## 10.6 ##
##########
#(a)
foo <- 5
bar <- c(2,3,1.1,4,0,4.1,3)
##(i)
loop2.result <- rep(NA,length(bar))
condition <- TRUE
counter <- 1
while(condition){
  temp <- foo/bar[counter]
  if(is.finite(temp)){
    loop2.result[counter] <- temp
  } else {
    condition <- FALSE
  }
  counter <- counter+1
}
loop2.result
##(ii)
loop3.result <- ifelse(is.finite(foo/bar),foo/bar,NA)
loop3.result
#(b)
mynumbers <- c(4,5,1,2,6,2,4,6,6,2)
##(i)
mylist <- list()
for(i in 1:length(mynumbers)){
  if(mynumbers[i]<=5){
    mylist[[i]] <- diag(mynumbers[i])
  } else {
    break
  }
}
mylist
##(ii)
mylist <- list()
counter <- 0
repeat{
  counter <- counter+1
  if(counter<=length(mynumbers)){
    if(mynumbers[counter]<=5){
      mylist[[counter]] <- diag(mynumbers[counter])
    } else {
      break
    }
  } else {
    break
  }
}
mylist
#(c)
counter <- 0
reslist <- list()
for(i in 1:length(matlist1)){
  for(j in 1:length(matlist2)){
    counter <- counter+1
    if(ncol(matlist1[[i]])!=nrow(matlist2[[j]])){
      reslist[[counter]] <- "not possible"
      next
    }
    reslist[[counter]] <- matlist1[[i]]%*%matlist2[[j]]
  }
}
reslist
##(i)
matlist1 <- list(matrix(1:4,2,2),matrix(1:4),matrix(1:8,4,2))
matlist2 <- matlist1
##(ii)
matlist1 <- list(matrix(1:4,2,2),matrix(2:5,2,2),matrix(1:16,4,2))
matlist2 <- list(matrix(1:8,2,4),matrix(10:7,2,2),matrix(9:2,4,2))
