#' Calculate Root Mean Square Predictive Error of a Synthetic Control Analysis
#' 
#' This function calculates the root mean square predictive error (RMSPE) of a synthetic control analysis over a user specified time period.  Predictive error refers to the gap between the treated unit's observed outcome and the synthetic control's predicted outcome.  See Abadie, Diamond, Hainmueller (2010, 2015) for the role of this statistic in the synthetic control method.
#' 
#' @param dataprep The output of a call to \code{dataprep}
#' @param synth The output of a call to \code{Synth}
#' @param begin_time A numeric giving the beginning of the period over which to calculate the RMSPE
#' @param end_time A numeric giving the end of the period over which to calculate the RMSPE
#' 
#' @return A scalar containing the RMSPE over the given period
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
#' @seealso \code{\link{SynthErrorRatios}}
#' 
#' @examples
#' 
#' ##Example: Hainmueller and Diamond's Toy panel dataset
#'
#' #load data
#' data(synth.data)
#'
#' ## create matrices from panel data that provide inputs for fitMultiSynth()
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
#'  SynthRMSPE(dataprep.out, synth.out, 1991, 1996)
#' 
#' @rdname SynthRMSPE
#' @export
SynthRMSPE <- function(dataprep, synth, begin_time, end_time){
  time <- row.names(dataprep$Y1plot) %in% as.character(begin_time : end_time)
  gaps <- dataprep$Y1plot - (dataprep$Y0plot %*% synth$solution.w)
  gaps.sq <- gaps^2
  mean.gaps <- mean(gaps.sq[time])
  return(sqrt(mean.gaps))
}