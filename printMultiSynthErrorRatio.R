print.MultiSynthErrorRatio <- function(obj, sorted = FALSE){
  
  p <- sum(obj >= obj[1])/length(obj)
  #sort them from lowest to highest
  if(sorted){ to_print <- sort(obj) } else{ to_print <- obj }
  
  cat(" ************ Multi Synth ", comment(obj)[1], " RMSPE Ratios ********** \n", sep="")
  sapply(to_print, function(value){
    cat( names(to_print)[to_print == value], ": ", value, "\n", sep = "")
  })
  if(comment(obj)[1] == "Placebos"){
    cat(" ************ \n Exact p-value: ", p, sep = "" )
  }
  return()
}