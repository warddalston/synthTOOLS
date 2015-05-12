lags <- function(data, unit.id, var, two.lags=FALSE, three.lags=FALSE, lag.length=1){
  lag1 <- sapply(1 : nrow(data), function(x){
    if(x %in% seq(1, lag.length, 1)){ NA } else {
      if(data[x, unit.id] != data[x - lag.length, unit.id]){
        NA
      } else { data[x - lag.length, var] }
    } #close else
  }) #close apply 
  
  if(two.lags){
    lag2 <- lags(data, unit.id, var, two.lags=FALSE, three.lags=FALSE, lag.length=2)
  } # close if
  if(three.lags){
    lag3 <- lags(data, unit.id, var, two.lags=FALSE, three.lags=FALSE, lag.length=3)
  } #close if
  
  
  #if both two and three lags are both false
  if(!(two.lags | three.lags)){
    return(lag1)
  }#close if 
  
  #if only two lags
  if(two.lags & !three.lags){
    out <- cbind(lag1,lag2)
    colnames(out) <- paste(var, c("lag1", "lag2"), sep="_")
    return(out)
  } # close if
  
  #if three lags
  if(two.lags & three.lags){
    out <- cbind(lag1, lag2, lag3)
    colnames(out) <- paste(var,c("lag1", "lag2", "lag3"), sep="_")
    return(out)
  } # close if
  
} #close function 
