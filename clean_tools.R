


#Create a function which calculates the missing values ratio
missing.ratio <-function(x){
  r <- sum(is.na(x))/length(x)
  return(r)
}