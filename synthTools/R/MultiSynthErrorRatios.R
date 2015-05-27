#' Calculate Root Mean Square Predictive Errors for Multiple Synthetic Controls
#' 
#' This function calculates three statistics important for assessing the goodness of fit and significance of synthetic controls for objects containing data on several synthetic control fits, such as a \code{\link{MultiSynth}} object. It is not intended to be called directly by the user, but is instead called by \code{\link{fitMultiSynth}}.
#' 
#' @usage MultiSynthErrorRatios(MultiSynthPrep_obj, MultiSynth_obj,
#' treatment_time, input)
#' 
#'  @param MultiSynthPrep_obj A list of outputs from the dataprep function
#'  @param MultiSynth_obj A list of outputs from the synth function
#'  @param treatment_time A numeric giving the time treatment is administered
#'  @param input The output of a call to the function \code{dataprep}, containing the time periods for each of the analyses in the MultiSynthPrep_obj and MultiSynth_obj
#'  
#'  @details The three statistics implemented by this function are the pretreatment Root Mean Square Predictive Error (RMSPE), the posttreatment RMSPE, and the ratio of post- to pretreatment RMSPE.  As described in Abadie, Diamond, Hainmueller (2010, 2014), these statistics allow the reasercher to investigate both the goodness of fit and significance of synthetic control analyses. 
#'  
#' Pretreatment RMSPE measures goodness of fit in the pre-treatment period; typically, lower values imply a better fit.  Posttreatment RMSPE measures the divergence between the treated unit and the synthetic control after treatment, and in effect, measures how large the effect of treatment is.  The ratio of these two quantities combines these elements, taking on large values only when the fit is good and the effect is large.  
#' 
#' The ratios can then be used in combination with placebo analyses, as implmemented by \code{\link{fitMultiSynth}}, to determine the signifiance of a synthetic control analysis.  The logic is that placebo synthetic controls should not have large RMSPE ratios, because no treatment is administered to these units.  As such, if the treated case has a large RMSPE ratio relative to placebo cases, this serves as evidence that the treatment has had an impact on the outcome.   
#' 
#' @return A list with the elements
#' \itemize{
#' \item{PreErrors}{A vector of pre-treatment RMSPE values}
#' \item{PostErrors}{A vector of post-treatment RMSPE values}
#' \item{Ratios}{A vector of post to pre-treatment RMSPE ratio values}
#' }
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{fitMultiSynth}}
#' @seealso \code{\link{SynthRMSPE}}
#' 
#' @examples
#' 
#' ##Example: Hainmueller and Diamond's Toy panel dataset
#'
#' load data
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
#'  fitMultiSynth.out <- fitMultiSynth(dataprep.out, treatment_time = 1991)
#'  
#'  MultiSynthErrorRatios(fitMultiSynth.out@preps, fitMultiSynth.out@fits, 1991, 1996, fitMultiSynth.out@input)
#' 
#' @rdname MultiSynthErrorRatios
#' @export
MultiSynthErrorRatios <- function(MultiSynthPrep_obj, MultiSynth_obj, treatment_time, input){
  
  pre_period <- input$tag$time.plot[input$tag$time.plot < treatment_time]
  post_period <- input$tag$time.plot[input$tag$time.plot >= treatment_time] 
  
  #Calculate the Pre- error ratios here
  PreErrors <- mapply(SynthRMSPE, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(begin_time = pre_period[1], end_time = pre_period[length(pre_period)]), SIMPLIFY = TRUE)
  
  PostErrors <- mapply(SynthRMSPE, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(begin_time = post_period[1], end_time = post_period[length(post_period)]), SIMPLIFY = TRUE)
  
  Ratios <- PostErrors/PreErrors
  
  out <- list(PreErrors = PreErrors, PostErrors = PostErrors, Ratios = Ratios)
  
  return(out)
}
