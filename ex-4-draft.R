Neighbours<- function(x,i,j){ 
  living<-0
  if( (i-1)>0 & (j-1)>0) living<- living + x[i-1,j-1]
  if( (i-1)>0 & (j)>0) living <- living + x[i-1,j]
  if( (i-1)>0 & (j+1)<6) living <- living + x[i-1,j+1]
  if( (i)>0 & (j-1)>0) living <- living + x[i,j-1]
  if( (i+1)<6 & (j+1)<6) living <- living + x[i,j+1]
  if( (i+1)<6 & (j-1)<0) living <- living + x[i+1,j-1]
  if( (i+1)<6 & (j)>0)  living <- living + x[i+1,j]
  if( (i+1)<6 & (j+1)<6) living <- living+ x[i+1,j+1]
  return(living)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
y <- matrix(0,5,5)
for(i in 1:5){
  for(j in 1:5){
    if(x[i,j]==0){
      if(Neighbours(x,i,j)==3) y[i,j]<-1
    }
    if(x[i,j]==1){
      count<- Neighbours(x,i,j)
      if((count==2)|(count==3)) y[i,j]<-1
    }
  }
}
y
sum(y)
