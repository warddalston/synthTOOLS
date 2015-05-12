print.MultiSynthErrorRatio <- function(obj){
  # to_print <- sort(obj)
  to_print <- obj
  cat(" ************ Placebo RMSPE ********** \n")
  sapply(to_print, function(value){
    cat( names(to_print)[to_print == value], ": ", value, "\n", sep = "")
  })
  return()
}