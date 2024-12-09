library(EL)
phi<-0.5 #AR(1) coefficient
shift<-1 #change-point shift
nn<-1000 #number of data of the AR process
MM<-c(10,15,20) #different block size 
NN<-c(100,200,400) #different sample size;
lambda<-0.5 #location of a change-point ([0,1])
u<-10 # u - threshold for merging very close intervals where pp.values<0.05 

for (n in 1:length(NN)){
  N<-NN[n]
  for (m in 1:length(MM)){
    set.seed(4) #fixed set.seed for the replicability
    M<-MM[m]
    
    pp_bxp<-c()
    
    for(i in 1:1000)
    {
      xt <- arima.sim(list(order=c(1,0,0), ar=phi), nn)
      zt<-c(xt[1:(nn*lambda)],xt[(nn*lambda+1):nn]+shift)

      pp.values<-c()
      for (j in 1:(nn-2*N))
        {
          F1.block<-zt[j:(j+N-1)]
          F2.block<-zt[(j+N):(j+2*N-1)]
          pp.values[j]<-BEL.means(F1.block,F2.block,M_1=M, M_2=M, Delta = 0)$p.value
        }
      pp.values<- p.adjust(pp.values, method="BH") # p-values adjustment  
      
	  pv<-which(pp.values<0.05)+(N+1)
      pv_diff<-c(0,which(diff(pv)>u),length(pv)) #merging intervals where pp.values<0.05 if they are too close
      
	  pp<-c()
      for (k in 1:(length(pv_diff)-1)){
        pp[k]<-round(mean(pv[(pv_diff[k]+1):pv_diff[k+1]]),0) #finding change-points
      }
      
      pp_bxp<-c(pp_bxp,pp)
      }
    write.csv(pp_bxp,paste("AR1_05_shift1_1000_chptn_N_",N,"_M_",M,"p_adjust_bonferroni.csv",sep=""),row.names=F) #output
    print(m)
  }
}

