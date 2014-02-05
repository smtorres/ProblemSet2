##ProblemSet2
##===========  
##PROBLEM 1
#Function to get the Statistics Leemis and Cho-Gains
# and the distribution of the first digit of the numbers in a set
#Input: x= Vector or matrix with electoral results
#       leemis.m = TRUE to obtain Leemis statistic
#       cho.gains = TRUE to obtain Cho-Gains statistic
#       DEFAULT: No statistic

benford <- function(x, leemis.m=FALSE, cho.gains=FALSE){ 
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
    listr<-list(leemis, chogains, cbind(nums, totals))
    print(paste("Leemis=", leemis))
    print(paste("Cho-Gains=", chogains))
    return(listr)
  }
  else if(leemis.m==FALSE & cho.gains==TRUE){
    listr<- list(chogains, cbind(nums, totals))
    print(paste("Cho-Gains=", chogains))
    return(listr)
  }
  else if(leemis.m==TRUE & cho.gains==FALSE){
    listr<- list(leemis, cbind(nums, totals))
    print(paste("Leemis=", leemis))
    return(listr)
  }
  else if (leemis.m==FALSE & cho.gains==FALSE){
    listr<-list(cbind(nums,totals))
    print("No statistics requested")
    return(listr)    
  } else{}
}

## PROBLEM 2
##FUNCTION "print.benford"
## Returns a table with the values of Leemis' m and Cho-Gains' d statistics
## and determines if they are significant in order to reject the null
##hypothesis "no fraud" from the Benford Distribution 
## Input: x= vector or matrix of electoral results
## Output: table with the values for each m and d, significance at 
# 0.01, 0.5 and 0.1
print.benfords<-function(x) { 
  stats<-benford(x, leemis.m=TRUE, cho.gains=TRUE)
  Statistic<- c("Leemis'm", "Cho-Gains' d")
  Legend<- c("CI: *0.10, **0.05, ***0.01", "","")
  Value<-c(stats[[1]], stats[[2]])
  mystars.l <- ifelse(stats[[1]] >= 1.212, "***", 
                ifelse((stats[[1]]<1.212 && stats[[1]]>=967), "** ", 
                  ifelse((stats[[1]]>=0.851 && stats[[1]] < 967), "* ", " ")))
  mystars.c <- ifelse(stats[[2]] >= 1.569, "***", 
                  ifelse((stats[[2]] >= 1.33), "** ", 
                    ifelse(stats[[2]] >= 1.212, "* ", " ")))
  Significance<-c(mystars.l, mystars.c)
  results<-cbind(Statistic, Value, Significance)
  results<-rbind(results,Legend)
  return(results)
  
}
