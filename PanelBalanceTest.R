Panel_Balance_test <- function(data, country){
  bad <- vector()
  for(i in unique(data$ctr_n)){
    if(length(data$yr[ data$ctr_n==i])!= sum( data$ctr_n == country) ){
      print(i)
      print(length(data$yr[data$ctr_n==i]))
      bad <- c(bad,i)
    }
  }
  return(bad)
}