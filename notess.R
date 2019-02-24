note<-read.csv('banknote.csv')
library(tidyverse)
notes<-data.frame(A=note[,1],B=note[,2],C=note[,3],D=note[,4],E=note[,5],Label=0,
                  n_Label=0)
head(notes)
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
notes$A<-normalize(notes$A)
notes$B<-normalize(notes$B)
notes$C<-normalize(notes$C)
notes$D<-normalize(notes$D)
head(notes)
                  
ggplot(data=notes)+geom_point(mapping=aes(x=A,y=B),alpha=notes$Label)+geom_point(mapping=aes(x=A,y=C),shape=notes$Label)+geom_point(mapping=aes(x=A,y=D),shape=notes$Label)

x1=(1/2)-(1/2)/2
x2=2*(1/2)-(1/2)/2
v1<-vector()
v2<-vector()
for(p in 1:4)
{
  v1[p]=x1
  v2[p]=x2
}

for(p in 1:30)
{
  
  for(i in 1:length(notes$A))
  {
    distance1=((v1[1]-notes$A[i])**2+(v1[2]-notes$B[i])**2+
              (v1[3]-notes$C[i])**2+(v1[4]-notes$D[i])**2)
    
    distance2=((v2[1]-notes$A[i])**2+(v2[2]-notes$B[i])**2+
              (v2[3]-notes$C[i])**2+(v2[4]-notes$D[i])**2)
    
    if(distance1>=distance2)
    {
      notes$Label[i]=0
    }
    else
    {
      notes$Label[i]=1
    }
  }
  
  f<-0
  for(j in 1:length(notes$A))
  {
    if(notes$Label[j]==notes$n_Label[j])
    {
      f<-f+1
    }
  }
  if(f==length(notes$A))
  {
    break
  }
  notes$n_Label<-notes$Label
  
  v1[1]<-mean(notes$A[notes$Label==0])
  v1[2]<-mean(notes$B[notes$Label==0])
  v1[3]<-mean(notes$C[notes$Label==0])
  v1[4]<-mean(notes$D[notes$Label==0])

  v2[1]<-mean(notes$A[notes$Label==1])
  v2[2]<-mean(notes$B[notes$Label==1])
  v2[3]<-mean(notes$C[notes$Label==1])
  v2[4]<-mean(notes$D[notes$Label==1])

}

pk<-kmeans(notes,2)
table(notes$Label,notes$E)
table(pk$cluster,notes$E)

paul<-double(200)
for(k in 1:200)
{
  pk<-kmeans(notes,k)
  notes$Label<-pk$cluster
  
  for(i in 1:k)
  {
    l=pk$centers[i,]
    
    for(j in 1:length(notes$A))
    {
      if(notes$Label[j]==i)
      {
        paul[k]=paul[k]+(l[1]-notes$A[j])**2+
          (l[2]-notes$B[j])**2+(l[3]-notes$C[j])**2+(l[3]-notes$D[j])**2
      }
    }
  }
  paul[k]=paul[k]/length(notes$A)
}
x<-200
plot(1:x,paul,type='l')
