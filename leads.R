leads <- function(data, unit.id, var, two.leads=FALSE, three.leads=FALSE, lead.length=1){
  lead1 <- sapply(1 : nrow(data), function(x){
    if(x %in% seq(nrow(data), nrow(data) - lead.length + 1, -1)){ NA } else {
      if(data[x, unit.id] != data[x + lead.length, unit.id]){
        NA
      } else { data[x + lead.length, var] }
    } #close else 
  }) #close apply 
  
  if(two.leads){
    lead2 <- leads(data, unit.id, var, two.leads=FALSE, three.leads=FALSE, lead.length=2)
  } # close if
  if(three.leads){
    lead3 <- leads(data, unit.id, var, two.leads=FALSE, three.leads=FALSE, lead.length=3)
  } #close if
  
  
  #if both two and three leads are both false
  if(!(two.leads | three.leads)){
    return(lead1)
  }#close if 
  
  #if only two leads
  if(two.leads & !three.leads){
    out <- cbind(lead1,lead2)
    colnames(out) <- paste(var, c("lead1", "lead2"), sep="_")
    return(out)
  } # close if
  
  #if three leads
  if(two.leads & three.leads){
    out <- cbind(lead1, lead2, lead3)
    colnames(out) <- paste(var,c("lead1", "lead2", "lead3"), sep="_")
    return(out)
  } # close if
  
} #close function 
