#####This function takes one of my data frames and uses avg.maker and a five years object to return a data.frame

avgData.maker <- function(data, five_years){
  avgData <- NULL
  for(i in unique(data$ctr_n)){  
    
    what <- mapply(avg.maker,five_years[1:(length(five_years)-1)],five_years[2:length(five_years)],MoreArgs=list(country=i,dataframe=data,country.var="ctr_n",year.var="yr"),SIMPLIFY=FALSE)
    out <- NULL
    for(i in 1: length(what)){
      out <- rbind(out,what[[i]])
    }
    avgData <- rbind(avgData,out)
  }
  
  #Quick fix for a variable that got turned into a factor somehow 
  avgData$ctr_n <- as.character(avgData$ctr_n)
  return(avgData)
}
