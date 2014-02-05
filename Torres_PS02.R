##ProblemSet2
##===========  
##PROBLEM 1
#Function to get the Statistics Leemis and Cho-Gains
# and the distribution of the first digit of the numbers in a set
#Input: x= Vector or matrix with electoral results
#       leemis.m = TRUE to obtain Leemis statistic
#       cho.gains = TRUE to obtain Cho-Gains statistic
#       DEFAULT: No statistic
# Output: List including the statistics requested and the distribution of digits

benford <- function(x, leemis.m=FALSE, cho.gains=FALSE){ 
  x<-as.vector(x)   #If the input is a matrix, it converts it to vector
  vec<-1:length(x)  
  for (i in 1:length(x)){
    vec[i]<-as.numeric(head(strsplit(as.character(x),'')[[i]],n=1)) 
  }         # Vector containing the first digit of each element in vector x
  totals<-rep(0,9)
  leemvec<-chovec<-numeric(9)
  for(j in 1:9){ 
    totals[j] <- (sum(vec==j))/length(vec) #Proportions of each digit
    leemvec[j]<- totals[j]-log10(1+ (1/j)) #First step of Leemis calculation
    chovec[j]<-(totals[j]-log10(1+ (1/j)))^2 #First step of Cho-Gains Calculation
  }
  nums<-1:9
  leemis<-max(leemvec)*sqrt(length(vec))  #Final Leemis m
  chogains<- sqrt(sum(chovec))*sqrt(length(vec)) # Final Cho-Gains d
  if(leemis.m==TRUE & cho.gains==TRUE){
    listr<-list(leemis, chogains, cbind(nums, totals))
    print(paste("Leemis=", leemis))
    print(paste("Cho-Gains=", chogains))
    return(listr) #Conditions in INPUT, statistics to return
  }
  else if(leemis.m==FALSE & cho.gains==TRUE){
    listr<- list(chogains, cbind(nums, totals))
    print(paste("Cho-Gains=", chogains))
    return(listr) #Conditions in INPUT, statistics to return
  }
  else if(leemis.m==TRUE & cho.gains==FALSE){
    listr<- list(leemis, cbind(nums, totals))
    print(paste("Leemis=", leemis))
    return(listr) #Conditions in INPUT, statistics to return
  }
  else if (leemis.m==FALSE & cho.gains==FALSE){
    listr<-list(cbind(nums,totals))
    print("No statistics requested")
    return(listr)    #Conditions in INPUT, statistics to return
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
  Legend<- c("Significance: *0.10, **0.05, ***0.01", "","")
  Value<-c(stats[[1]], stats[[2]])
  mystars.l <- ifelse(stats[[1]] >= 1.212, "***", 
                ifelse((stats[[1]]<1.212 && stats[[1]]>=967), "** ", 
                  ifelse((stats[[1]]>=0.851 && stats[[1]] < 967), "* ", "No fraud")))
  #Conditions of significance for Leemis' m
  mystars.c <- ifelse(stats[[2]] >= 1.569, "***", 
                  ifelse((stats[[2]] >= 1.33), "** ", 
                    ifelse(stats[[2]] >= 1.212, "* ", "No fraud")))
  #Conditions of significance for Cho-Gains' d
  Significance<-c(mystars.l, mystars.c)
  results<-cbind(Statistic, Value, Significance)
  results<-rbind(results,Legend)
  tresults<-as.table(results, row.names=c("", "", ""))
  return(tresults)
  
}

##PROBLEM 3
## Data set where Benford law is NOT met
setwd("/Users/michelletorres/Dropbox/SEMESTER2/R-Programming/ProblemSet2")
benfordfalse<-read.table("president2012.txt", header=TRUE, sep="|")
votres<-cbind(benfordfalse$PAN, benfordfalse$PRI, benfordfalse$PRD, benfordfalse$PVEM,
              benfordfalse$PT, benfordfalse$MC, benfordfalse$PANAL)
print.benfords(votres)
## Function to graph the distribution
graph.benford<-function(x){
  x<-as.vector(x)
  vec<-1:length(x)
  for (i in 1:length(x)){
    vec[i]<-as.numeric(head(strsplit(as.character(x),'')[[i]],n=1)) 
  }
  return(truehist(vec))
}
graph.benford(votres)

## Data set where Benford law is met
benfordtrue <- c() 
for(i in 1:100){ 
    benfordtrue[i] <- 4^i 
  }
benfordtrue
print.benfords(benfordtrue)
graph.benford(benfordtrue)

##FUNCTION test.function
##Function that compares a correct calculation of benford distribution
## and statistics with a new method to calculate those elements 
## INPUT: a vector or matrix with electoral results
## OUTPUT: TRUE, the new method matches the correct calculation
##         FALSE, the new method DOES NOT match the correct calculation
##            + It specifies which is the incorrect step (distribution or statistic)
test.function<-function(x){
  benford.t<-benford(x, leemis.m=TRUE, cho.gains=TRUE) #Save True Benford list
  invisible(benford.t)
  x<-as.vector(x)
  vece<-1:length(x)                         #New calculation
  for (i in 1:length(x)){
    vece[i]<-as.numeric(head(strsplit(as.character(x),'')[[i]],n=1)) 
  }
  totals2<-rep(0,9)
  leemvece2<-chovece2<-numeric(9)
  for(j in 1:9){ 
    totals2[j] <- (sum(vece==j))/(length(vece))*2      #Correct calculation
    leemvece2[j]<- totals2[j]-log10(1+ (1/j))      #Correct leemis
    chovece2[j]<-(totals2[j]-log10(1+ (1/j)))^2    #Correct cho
  }
  numse1<-1:9
  leemise2<-max(leemvece2)*sqrt(length(vece))     #Correct final leemis
  chogainse2<- sqrt(sum(chovece2))*sqrt(length(vece)) #Correct final cho
  
  ##TESTS (Comparing correct calculation with new one)
  dist.test<-sum(benford.t[[3]][,2]==totals2)
  stat.test<-sum(c(benford.t[[1]]==leemise2, benford.t[[2]]==chogainse2))
  output2<-c(TRUE, FALSE)
  if(dist.test!=9){
    print("Error in Benford distribution")
    return(output2[2])
  }
  else if (dist.test==9 && stat.test!=2){
    print("Error in statistics calculation")
    return(output2[2])
  }
  else{
    print("You passed the test")
    return(output2[1])
  }
}
#Test function in Dataset where Benford Law is not met
test.function(votres)