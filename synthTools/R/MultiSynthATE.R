#' Calculate the Mean Gap in Outcome for Multiple Synthetic Control Fits
#' 
#' This function calculates the mean gap in outcome between the treated unit and the synthetic control unit over a user specified period for multiple synthetic control fits.  It requires the user to provide a list of \code{\link{dataprep}} outputs and \code{\link{synth}} outputs, as well as to provide the beginning and end time periods over which to calculate the mean gap.  It is not designed to be used directly by the user, by rather, is called as part of \code{\link{fitMultiSynth}}.  
#'  
#'  @param MultiSynthPrep_obj A list of outputs from the dataprep function
#'  @param MultiSynth_obj A list of outputs from the synth function
#'  @param begin_time A numeric containing the beginning of the period over which to calculate the average gap
#'  @param end_time A numeric containing the end of the period over which to calculate the average gap
#'  
#'  @details This function is called automatically by \code{\link{fitMultiSynth}}.  However, it can also be called independently.  To do this with a \code{\link{MultiSynth}} object, the user should give the "preps" slot as the first arguement, and "fits" slot as the second argument. 
#'  
#'  Note that when the user provided time period corresponds to the entire post-treatment period, the quantity calculated by this function is the average treatment effect (ATE).
#'  
#'  @return A named vector containing the estimated mean gaps
#'  
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{fitMultiSynth}}
#' @seealso \code{\link{SynthMeanEffect}}
#' 
#' @examples
#' 
#' \dontrun{
#' #Example: Hainmueller and Diamond's Toy panel dataset
#'
#' #load data
#' data(synth.data)
#'
#' # create matrices from panel data that provide inputs for fitMultiSynth()
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
#'  fitMultiSynth.out <- fitMultiSynth(dataprep.out, treatment_time = 1991)
#'  
#' MultiSynthMeanEffect(fitMultiSynth.out@@preps, 
#' fitMultiSynth.out@@fits, 1991, 1996)
#' }
#' 
#' @rdname MultiSynthMeanEffect
#' @export
MultiSynthMeanEffect <- function(MultiSynthPrep_obj, MultiSynth_obj, begin_time, end_time){
  
  #Calculate the pre treatment error ratios here
  out <- mapply(SynthMeanEffect, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(begin_time = begin_time, end_time = end_time), SIMPLIFY = TRUE)

  return(out)
}