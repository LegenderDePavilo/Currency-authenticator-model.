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
length(paul)  
x<-200
plot(1:x,paul,type='l')