#' Calculate Root Mean Square Predictive Errors for a Synthetic Control Analysis
#' 
#' This function calculates three statistics important for assessing the goodness of fit and significance of a synthetic controls analysis.  These are the pretreatment root mean square predictive error (RMSPE), posttreatment RMSPE, and post/pre treatment RMSPE ratio, as described in Abadie, Diamond, and Hainmueller (2015).    
#'  
#'  @param dataprep Ouput from a call to \code{dataprep}
#'  @param synth Output from a call to \code{synth}
#'  @param treatment_time A numeric giving the time treatment is administered.
#'  
#'  @details  As described in Abadie, Diamond, Hainmueller (2010, 2014), these statistics allow the reasercher to investigate both the goodness of fit and significance of synthetic control analyses. Pretreatment RMSPE measures goodness of fit in the pre-treatment period; typically, lower values imply a better fit.  Posttreatment RMSPE measures the divergence between the treated unit and the synthetic control after treatment, and in effect, measures how large the effect of treatment is.  The ratio of these two quantities combines these elements, taking on large values only when the fit is good and the effect is large.  
#'    
#' 
#' @return A list with the elements
#' \itemize{
#' \item \code{PreErrors} {A numeric containing the pre-treatment RMSPE value}
#' \item \code{PostErrors} {A numeric containing the post-treatment RMSPE value}
#' \item \code{Ratios} {A numeric containing the post-to-pretreatment RMSPE ratio value}
#' }
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
#' @seealso \code{\link{SynthRMSPE}}
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
#'  SynthErrorRatios(dataprep.out, synth.out, 1991)
#' 
#' @rdname SynthErrorRatios
#' @export
SynthErrorRatios <- function(dataprep, synth, treatment_time){
  pre_period <- dataprep$tag$time.plot[ ! dataprep$tag$time.plot >= treatment_time ]
  pre_error <- SynthRMSPE(dataprep, synth, pre_period[1], pre_period[ length(pre_period) ])
  post_period <- dataprep$tag$time.plot[ dataprep$tag$time.plot >= treatment_time ]
  post_error <- SynthRMSPE(dataprep, synth, post_period[1], post_period[ length(post_period) ])
  out <- list(pre = pre_error, post = post_error, ratio = post_error/pre_error)
  class(out) <- "SynthErrorRatios"
  return(out)
}
