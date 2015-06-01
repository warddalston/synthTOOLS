#' Print the RMSPE values for a Synthetic Control Analysis
#' 
#' This function prints the pre-treatment root mean square predictive error RMSPE values, post-treatment RMSPE, and the ratio of these two values in a synthetic control analysis, labelling each statistic. 
#' 
#' @param x An object of class SynthErrorRatios
#' @param digits The number of digits to print, defaults to 3
#' @param ... Additional arguments passed to other functions
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
print.SynthErrorRatios <- function(x, digits = 3, ...){
  cat("************ Pre-Treatment RMSPE: ", round(x$pre, digits), "\n", sep = "")
  cat("************ Post-Treatment RMSPE: ", round(x$post, digits), "\n", sep = "")
  cat("************ Pre-Post RMSPE Ratio: ", round(x$ratio, digits), "\n", sep = "")
  return(invisible())
}
