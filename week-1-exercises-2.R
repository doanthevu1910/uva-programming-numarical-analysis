getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 1")

rm(list = ls())

#3.1

mymat <- matrix(data = c(4.3,3.1,8.2,3.2,0.9,1.6,6.5), nrow = 4, ncol = 2, byrow = TRUE); mymat

dim(mymat[-2,])

mymat[,2] <- sort(x=mymat[,2], decreasing = FALSE)

matrix(data = mymat[-4,-1])

mymat2 <- mymat[3:4,]

mymat[c(4,1), 2:1] <- -0.5*diag(mymat2)

#3.2

A <- matrix(data = c(1,2,2,4,7,6), 3, 2, byrow = TRUE); A

B <- matrix(data = c(10, 20, 30, 40, 50, 60), 3, 2, byrow = TRUE); B

2/7*(A - B)

A <- matrix(c(1, 2, 7), 3, 1); A

B <- matrix(c(3,4,8), 3, 1); B

A%*%B

t(A)%*%B

t(B)%*%(A%*%t(A))

(A%*%t(A))%*%t(B)

((B%*%t(B))+(A%*%t(A))-100*diag(3))^-1

#3.3

AR <- array(data = seq(4.8, 0.1, l=48), dim = c(4,2,6)); AR

BR <- AR[c(4,1),2,]; BR

CR <- array(data = rep(x = BR[2,], times = 4), dim = c(2,2,2,3)); CR

DR <- AR[,,-6]; DR

DR[c(2,4), 2, c(1,3,5)] <- 99; DR

#4.1

a <-  c(6,9,7,3,6,7,9,6,3,6,6,7,1,9,1)

a == 6

a >= 6

a < 6+2

a != 6

b <- a[-(1:3)]; b

AR <- array(data = b, dim = c(2,2,3)); AR

AR <= 7

AR + 2 <= 7

diag(10) == 0

any(AR <= 7)

all(AR <= 7)

any(diag(diag(10))==0)

#4.2

foo <- c(7,1,7,10,5,9,10,3,10,8)

foo >= 5 | foo == 2

bar <- c(8,8,4,4,5,1,5,6,6,8)

(bar<=6)&(bar!=4)

(foo >= 5 | foo == 2)&((bar<=6)&(bar!=4))

baz <- foo + bar

(baz >= 14)&(baz != 15)

(baz/foo)>4 | (baz/foo) <= 2

#4.3

foo <- c(7,5,6,1,2,10,8,3,8,2)

bar <- foo[foo>=5]

foo[-which(x=foo>=5)]

baz <- matrix(data=bar,nrow=2,ncol=3,byrow=T)

baz[baz==8] <- baz[1,2]^2

all(baz<=25 && baz>4)

qux <- array(data=c(10,5,1,4,7,4,3,3,1,3,4,3,1,7,8,3,7,3),dim=c(3,2,3))

which(x = qux == 3 | qux == 4, arr.ind = T)

qux[qux<3|qux>=7] <- 100

foo[c(F,T)]

foo[c(0,1)]

#4.4

cat("\"The quick brown fox\n\tjumped over\n\t\tthe lazy dogs\"")

num1 <- 4; num2 <- 0.75

paste("The result of multiplying",num1,"by",num2,"is",num1*num2)

sub(pattern="tdavies",replacement="vtdoan",x="/Users/tdavies/Documents/RBook")

bar <- "How much wood could a woodchuck chuck"

baz <- paste(bar,"if a woodchuck could chuck wood")

gsub(pattern="wood",replacement="metal",x=baz)

foo <- "Two 6-packs for $12.99"

substr(x=foo,start=5,stop=10)=="6-pack"

substr(x=foo,start=19,stop=19) <- "0"; foo

#4.5

party <- rep("National",20)
party[c(1,4,12,15,16,19)] <- "Labour"
party[c(6,9,11)] <- "Greens"
party[c(10,20)] <- "Other"
party

sex <- rep("M",20)
sex[c(1,5:7,12,14:16)] <- "F"
sex

sex.fac <- factor(x=sex)
sex.fac
party.fac <- factor(x=party,levels=c("National","Labour","Greens","Maori","Other"))
party.fac

party.fac[sex.fac=="M"]

sex.fac[party.fac=="National"]

sex.newvals <- factor(x=c("M","M","F","F","F","M"))
sex.fac <- factor(x=levels(sex.fac)[c(sex.fac,sex.newvals)])
sex.fac

party.newvals <-factor(x=c("National","Maori","Maori","Labour","Greens","Labour"),levels=levels(party.fac))
party.fac <- factor(x=levels(party.fac)[c(party.fac,party.newvals)])
party.fac

conf <- c(93,55,29,100,52,84,56,0,33,52,35,53,55,46,40,40,56,45,64,31,10,29,40,95,18,61)
conf.fac <- cut(x=conf,breaks=c(0,30,70,100),include.lowest=TRUE,labels=c("Low","Moderate","High"))

conf.fac[party.fac=="Labour"]
conf.fac[party.fac=="National"] 
