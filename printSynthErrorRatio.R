#' Print the RMSPE values for a synthetic control analysis
#' 
#' THis function prints the RMSPE values in a synthetic control analysis, labelling each statistic. 
#' 
#' @usage print(obj, digits)
#' 
#' @param obj An object of class SynthErrorRatios
#' @param digits The number of digits to print, defaults to 3
#' 
#' @details This function is called silently anytime the user sends an object of class SynthErrorRatios directly to the console.  
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{SynthRMSPE}}
#' @seealso \code{\link{synth}}
#' @seealso \code{\link{dataprep}}
#' 
#' @rdname print.SynthErrorRatios
#' @export
print.SynthErrorRatios <- function(obj, digits = 3){
  cat("************ Pre-Treatment RMSPE: ", round(obj$pre, digits), "\n", sep = "")
  cat("************ Post-Treatment RMSPE: ", round(obj$post, digits), "\n", sep = "")
  cat("************ Pre-Post RMSPE Ratio: ", round(obj$ratio, digits), "\n", sep = "")
  return(invisible())
}
