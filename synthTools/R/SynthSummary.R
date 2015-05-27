#' Summarize the results of a Synthetic Control Analysis 
#' 
#' This function builds upon \code{\link{synth.tab}} to summarize the results of a synthetic control analysis.  It prints the weights assigned to covariates, average covariate values for the treated unit and the synthetic control unit, weights assigned to donor pool units, values of the pre and post treatment Root Mean Square Predictive Error (RMSPE), the ratio of posttreatment to pretreatment RMSPE, the average treatment effect, and the covariate loss (taken from the output of a call to \code{\link{synth}}.  
#' 
#'  The covariate and unit weights allow the user to assess which covariates and units are most important for the construction of the synthetic control.  The values taken by the synthetic control and the treated unit on predictors, as well as the covariate loss statistic, allow the user to determine the extent to which the synthetic control replicates the relevent observable features of the treated unit.  The RMSPE statistics allow the user to determine how well the synthetic control replicates the outcome trajectory in the pretreatment period and the extent of divergence between the treated and synthetic control unit in the post-treatment period.  The RMSPE ratio statistic combines these two, taking large values only when there is a good fit and a large treatment effect, as described in Abadie, Diamond, Hainmueller (2015).  
#'  
#'  All of the matrices and statistics printed are also returned invisibly.  
#'  
#'  @usage SynthSummary(dataprep, synth, treatment_time, digits = 3)
#'  
#'  @param dataprep The output of a call to dataprep
#'  @param synth The output of a call to synth.  This object should be created using the same dataprep output as given for the first arguement.  
#'  @param treatment_time A scalar giving the time period in which treatment is administered.
#'  @param digits A scalar giving the number of digits to print.  Defaults to three.  
#'  
#'  @details This function internally calls on several functions to create the matrices to print and estimate the statistics to print.  These are \code{\link{synth.tab}}, \code{\link{SynthErrorRatios}}, and \code{\link{SynthMeanEffect}}. 
#'  
#'    @return A list containing the following elements
#'    \itemize{
#'    \item{covariates}{A matrix containing the average values of the covariates for the treated unit, synthetic control, and donor pool, as well as weights assigned to the covariates in estimation}
#'    \item{units}{A matrix containing the unit names and weights assigned to them for the construction of the synthetic control unit}
#'    \item{RMSPEs}{A vector containing the three RMSPE statistics}
#'    \item{ATE}{The estimated ATE}
#'    \item{CovLoss}{The estimated covariate loss, taken from \code{synth}}
#'    }  
#'
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{synth}}
#' @seealso \code{\link{dataprep}}
#' @seealso \code{\link{synth.tab}}
#' @seealso \code{\link{SynthMeanEffect}}
#' @seealso \code{\link{SynthErrorRatios}}
#' 
#' @examples
#' 
#' #load data
#' data(synth.data)
#'
#' dataprep.out<-
#'  dataprep(
#'    foo = synth.data,
#'    predictors = c("X1", "X2", "X3"),
#'    predictors.op = "mean",
#'    dependent = "Y",
#'    unit.variable = "unit.num",
#'    time.variable = "year",
#'    special.predictors = list(
#'      list("Y", 1991, "mean"),
#'      list("Y", 1985, "mean"),
#'      list("Y", 1980, "mean")
#'    ),
#'    treatment.identifier = 7,
#'    controls.identifier = c(29, 2, 13, 17, 32, 38),
#'    time.predictors.prior = c(1984:1989),
#'    time.optimize.ssr = c(1984:1990),
#'    unit.names.variable = "name",
#'    time.plot = 1984:1996
#'  )
#'  
#'  synth.out <- synth(dataprep.out)
#'  
#'  SynthSummary(dataprep.out, synth.out, 1991)
#'  
#'  @rdname SynthSummary
#'  @export
SynthSummary <- function(dataprep, synth, treatment_time, digits = 3){
  
  RMSPE <- SynthErrorRatios(dataprep, synth, treatment_time) 
  ATE <- SynthMeanEffect(dataprep, synth, treatment_time, max(dataprep$tag$time.plot))
  tabs <- synth.tab(synth.res = synth, dataprep.res = dataprep, round.digit = digits)
  tab_print <- cbind(tabs$tab.pred, tabs$tab.v)
  colnames(tab_print) <- c("Treated", "Synthetic", "Sample Mean", "Weight")
  colnames(tabs$tab.w) <- c("Weight", "Name", "Number")
  
  cat(" ************ Covariate Values and Weights ************ \n")
  print(tab_print)
  cat( "\n************ Unit Weights ************ \n")
  print(tabs$tab.w)
  cat("\n")
  print(RMSPE)
  cat( "************ Average Treatment Effect: ", round(ATE, digits), "\n", sep = "")
  cat( "************ Covariate Loss: ", round(synth$loss.w, digits), sep = "")
  
  return(invisible(list(covariates = tab_print, units = tabs$tab.w, RMSPEs = RMSPE, ATE = ATE, CovLoss = synth$loss.w)))
  
}