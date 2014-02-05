ProblemSet2
===========

An initial blank repository for our second problem set.  Fork this and clone it to your own computer before starting.  
##PROBLEM 1
#Function to get the first digit of any number "x"
#Input: x= Vector or matrix with electoral results
first.num <- function(x){ 
  x<-as.vector(x)
  vec<-1:length(x)
  for (i in 1:length(x)){
  vec[i]<-as.numeric(head(strsplit(as.character(x),'')[[i]],n=1)) 
  print(vec)
  count<-table(vec)
}
return(count)
}
  

