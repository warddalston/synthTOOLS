print.SynthErrorRatios <- function(obj, digits = 3){
  cat(" ************ Pre Treatment RMSPE: ", round(obj$pre, digits), "\n", sep = "")
  cat(" ************ Post Treatment RMSPE: ", round(obj$post, digits), "\n", sep = "")
  cat(" ************ Pre-Post Treatment RMSPE Ratio: ", round(obj$ratio, digits), "\n", sep = "")
  return()
}
