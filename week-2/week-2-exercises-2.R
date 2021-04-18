getwd()

setwd("D:/UvA/Year 1/Block 5/Programming and Numerical Analysis/Week 2")

rm(list = ls())

#7.1
plot(-3:3,7:13,type="n",xlab="",ylab="")
text(x=0,y=10,labels="SOMETHING\nPROFOUND")
abline(v=c(-3,3),lty=2,lwd=4,col=8)
abline(h=c(7,13),lty=2,lwd=4,col=8)
arrows(x0=c(-2.5,-2.5,-2.5,2.5,2.5,2.5),y0=c(7.5,10,12.5,7.5,10,12.5),x1=c(-1,-1,-1,1,1,1),y1=c(9.5,10,10.5,9.5,10,10.5))

w <- c(55,85,75,42,93,63,58,75,89,67)
h <- c(161,185,174,154,188,178,170,167,181,178)
s <- c("female","male","male","female","male","male","female","male","male","f
emale")

plot(w,h,type="n",xlab="Weight (kg)",ylab="Height (cm)",main="Height against weight for 10 people")
points(w[s=="male"],h[s=="male"],pch=19)
points(w[s=="female"],h[s=="female"],pch=3,col=2)
legend("topleft",legend=c("male","female"),pch=c(19,3),col=c(1,2))

#7.2
w <- c(55,85,75,42,93,63,58,75,89,67)
h <- c(161,185,174,154,188,178,170,167,181,178)
s <- factor(c("female","male","male","female","male","male","female","male","male","f
emale"))

library(ggplot2)

qplot(w,h,color=s,shape=s,xlab="Weight (kg)",ylab="Height (cm)",main="Height against weight for 10 people") + geom_point(size=4)

#8.1

quakes
write.table(x=quakes[quakes$mag>=5,],file="q5.txt",sep="!",row.names=F)

file.choose()

q5.dframe <- read.table(file = "q5.txt", sep = "!",header = T)

install.packages("car")
library(car)
data(Duncan)

plot(Duncan$education[Duncan$prestige<=80],Duncan$income[Duncan$prestige<=80],xlim=c(0,100),ylim=c(0,100),xlab="Education",ylab="Income")
points(Duncan$education[Duncan$prestige>80],Duncan$income[Duncan$prestige>80],pch=19,col="blue")

png("dunc.png",width=500,height=500)

plot(Duncan$education[Duncan$prestige<=80],Duncan$income[Duncan$prestige<=80],xlim=c(0,100),ylim=c(0,100),xlab="Education",ylab="Income")
points(Duncan$education[Duncan$prestige>80],Duncan$income[Duncan$prestige>80],pch=19,col="blue")
legend("topleft",legend=c("prestige > 80","prestige <=80"),pch=c(19,1),col=c("blue","black"))
dev.off()
file.choose()

exer <- list(quakes,q5.dframe,Duncan)

dput(x=exer,file="Exercise8-1Data.txt")

list.of.dataframes <- dget("Exercise8-1Data.txt")
list.of.dataframes

x <- 1:20
y <- c(-1.49,3.37,2.59,-2.78,-3.94,-0.92,6.43,8.51,3.41,-8.23,-12.01,-6.58,2.87,14.12,9.63,-4.58,-14.78,-11.67,1.17,15.62)
ptype <- rep(NA,length(x=x))
ptype[y>=5] <- "too_big"
ptype[y<=-5] <- "too_small"
ptype[(x>=5&x<=15)&(y>-5&y<5)] <- "sweet"
ptype[(x<5|x>15)&(y>-5&y<5)] <- "standard"
ptype <- factor(x=ptype)
qplot(x,y,color=ptype,shape=ptype) + geom_point(size=4) +
  geom_line(mapping=aes(group=1),color="black",lty=2) +
  geom_hline(mapping=aes(yintercept=c(-5,5)),color="red") +
  geom_segment(mapping=aes(x=5,y=-5,xend=5,yend=5),color="red",lty=3) +
  geom_segment(mapping=aes(x=15,y=-5,xend=15,yend=5),color="red",lty=3)
ggsave(filename="elaborateqplot.tiff")
file.choose()
