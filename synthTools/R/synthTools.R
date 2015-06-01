#' synthTools Package
#' 
#' The synthTools main function is to implement placebo and leave-one-out analyses for synthetic control studies, as described in Abadie, Diamond, Hainmueller (2010, 2015).  These analyses require iterative fitting of several synthetic controls.  In a placebo analysis, synthetic controls are iteratively fit, with each a different donor pool unit being considered the treated unit in each fit.  In a leave-one-out analyses, either a single donor pool unit or a single covariate is excluded from the analysis in each iteration. The main function of the package, \code{\link{fitMultiSynth}} runs these analyses, and returns them in objects of class \code{\link{MultiSynth}}. only the output of \code{\link{dataprep}} and the time of treatment as required inputs.  Furthermore, \code{\link{fitMultiSynth}} automatically calculates a number of statistics useful for these analyses, and there are methods available for summarizing and plotting these results in \code{summary}, \code{plot}, \code{gaps.plot}, and \code{path.plot}.  Furthermore, synthTools also includes a function for fitting synthetic controls through cross validation (\code{\link{SynthCrossVal}}) and for calculating the root mean square predictive error (RMSPE) of a synthetic control analysis (\code{\link{SynthErrorRatios}}), as described by Abadie, Diamond, Hainmueller (2015).  Finally, \code{\link{SynthMeanEffect}} estimates average treatment effects (ATEs) and \code{\link{SynthSummary}} expands upon \code{\link{synth.tab}} to include RMSPE statistics and the ATE in the summary of a synthetic control fit.   
#' 
#' @name synthTools
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @examples
#' 
#' \dontrun{
#'  demo(fitMultiSynth)
#'  demo(SynthCrossVal)
#' }
#' 
NULL