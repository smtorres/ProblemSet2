##ProblemSet2
##===========  
##PROBLEM 1
#Function to get the first digit of any number "x"
#Input: x= Vector or matrix with electoral results

bendford <- function(x, leemis.m=FALSE, cho.gains=FALSE){ 
  x<-as.vector(x)
  vec<-1:length(x)
  for (i in 1:length(x)){
    vec[i]<-as.numeric(head(strsplit(as.character(x),'')[[i]],n=1)) 
  }
  totals<-rep(0,9)
  leemvec<-chovec<-numeric(9)
  for(j in 1:9){ 
    totals[j] <- (sum(vec==j))/length(vec)
    leemvec[j]<- totals[j]-log10(1+ (1/j))
    chovec[j]<-(totals[j]-log10(1+ (1/j)))^2
  }
  nums<-1:9
  leemis<-max(leemvec)*sqrt(length(vec))
  chogains<- sqrt(sum(chovec))*sqrt(length(vec))
  if(leemis.m==TRUE & cho.gains==TRUE){
    list<-list(leemis, chogains, cbind(nums, totals))
    print(paste("Leemis=", leemis))
    print(paste("Cho-Gains=", chogains))
    return(list)
  }
  else if(leemis.m==FALSE & cho.gains==TRUE){
    list<- list(chogains, cbind(nums, totals))
    print(paste("Cho-Gains=", chogains))
    return(list)
  }
  else if(leemis.m==TRUE & cho.gains==FALSE){
    list<- list(leemis, cbind(nums, totals))
    print(paste("Leemis=", leemis))
    return(list)
  }
  else if (leemis.m==FALSE & cho.gains==FALSE){
    list<-list(cbind(nums,totals))
    print("No statistics requested")
    return(list)    
  } else{}
}

