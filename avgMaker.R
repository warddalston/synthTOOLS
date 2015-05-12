#####This function creates the five or ten year average periods data
avg.maker <- function(country,five.yearU,five.yearL,dataframe,country.var,year.var){
  
  
  sub <- dataframe[dataframe[,country.var]==country & (dataframe[,year.var] < five.yearU & dataframe[,year.var] >= five.yearL),]
  
  small <- as.data.frame(lapply(1:ncol(sub), function(x){
    
    if(colnames(sub)[x] == year.var){ return(five.yearL) }
    if(colnames(sub)[x]=="id"){ return(paste(country, five.yearU-1, five.yearL, sep="_") )}
    if(class(sub[,x])%in%c("numeric","integer") & colnames(sub)[x] != year.var ) { 
      
      if(all(is.na(sub[,x]))){return(NA)} else {return(mean(sub[,x], na.rm=T) )}
      
    }
    if(colnames(sub)[x]==country.var) { return(country) }
  } #close apply function
  ) #close sapply
  ) #close as.data.frame
  colnames(small) <- colnames(sub)
  small$nelections <- nrow(sub)
  return(small) 
  #close apply
} #close function
