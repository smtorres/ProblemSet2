ProblemSet2
===========

An initial blank repository for our second problem set.  Fork this and clone it to your own computer before starting.  
##PROBLEM 1
#Function to get the first digit of any number "x"
#Input: x= Vector with length 1 or single number 
first.num <- function(x){ 
    as.numeric(head(strsplit(as.character(x),'')[[1]],n=1)) 
  } 

