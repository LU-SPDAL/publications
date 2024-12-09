library(EL)
p.values.blockwise<-function(data,N,M,u) #N - sample size; M - block size; u - threshold for merging very close intervals where pp.values<0.05 
{
  NN<-length(data)

  #p-values
  pp.values<-c()
  for (i in 1:(NN-2*N))
  {
    F1.block<-data[i:(i+N-1)];
    F2.block<-data[(i+N):(i+2*N-1)];
    pp.values[i]<-BEL.means(F1.block,F2.block,M_1=M, M_2=M, Delta = 0)$p.value
  }
  pp.values<- p.adjust(pp.values, method="BH") # p-values adjustment  
  
  ###
  #Identification of the change-points
  pv<-which(pp.values<0.05)+(N+1)
  pv_diff<-c(0,which(diff(pv)>u),length(pv)) #merging intervals where pp.values<0.05 if they are too close
  pp<-c()
  for (i in 1:(length(pv_diff)-1)){
    pp[i]<-round(mean(pv[(pv_diff[i]+1):pv_diff[i+1]]),0) #finding change-points
  }
  print(pp) #printing change-points
}
