print.MultiSynthCovBalance <- function(obj, sorted = FALSE, digits = 3){
  
  #sort them from lowest to highest
  if(sorted){ to_print <- sort(obj) } else{ to_print <- obj }
  
  cat(" ************ Multi Synth ", comment(obj)[1], " Covariate Loss ********** \n", sep="")
  sapply(to_print, function(value){
    cat( names(to_print)[to_print == value], ": ", round(value, digits), "\n", sep = "")
  })
  
  return()
}
