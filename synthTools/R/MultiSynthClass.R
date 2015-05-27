#' A MultiSynth (MS) Object
#' 
#' Objects of class \code{MultiSynth} are used in performing robustness and significance tests for Synthetic Control Analyses.  These objects contain the dataprep for a synthetic control analysis, basic information on the synthetic contol, the data for placebo or leave-one-out anlayses, the fits for these analysis, and several statistics used to analyze a synthetic control anlaysis. MultiSynth objects are the output of the the main functions of the \code{synthTOOLS} package, \code{\link{fitMultiSynth}}. They are created using the \code{initialize} function.      
#'
#' An object of the S4 class `MultiSynth' has the following slots:
#' \itemize{
#' \item \code{input} The output of the \code{dataprep} function.  
#' \item \code{preps} A list with the X0, X1, Z0, Z1, Y0plot, and Y1plot for each of the placebo/leave-one-out analyses
#' \item \code{fits} A list with the output of \code{synth} for each of the placebo/leave-one-out analyses
#' \item \code{treated} A character vector of length two giving the name and number of the treated unit (if names are not supplied, the number is repeated)
#' \item \code{treatment_time} A numeric vector of length one giving the time treatment is recieved
#' \item \code{PreRMSPE} A numeric vector giving the root mean square predictive error (RMSPE) in the pre-treatment period for the main analysis and each placebo/leave-one-out analysis. 
#' \item \code{PostRMSPE} A numeric vector giving the RMSPE in the post-treatment period for the main analysis and each placebo/leave-one-out analysis. 
#' \item \code{RMSPEratio} A numeric vector giving the ratio of post-treatment RMSPE to pre-treatment RMSPE for the main analysis and each placebo/leave-one-out analysis
#' \item \code{CovBalances} A numeric vector giving the covariate loss for the main analysis and each placebo/leave-one-out analysis.
#' \item \code{ATEs} A numeric vector giving the average treatment effect (ATE) for the main analysis and each placebo/leave-one-out analysis.
#' \item \code{p_value} A numeric given the exact p-value of getting an RMSPE ratio as high as the treated case, if choosing a case to analyze at random. Only in \code{PlaceboMS} objects.  
#' }
#'
#'@details MultiSynth objects allow for implementation of various significance and robustness checks for synthetic control analyses, as described in Abadie, Diamond, Hainmueller (2010) and Abadie, Diamond, Hainmueller (2015).  Currently, MultiSynth implements in-space placebo analyses, leave-one-out units analyses, and leave-one-out covariates analyses.  MultiSynth also refits the "main" analysis, and records this as the first element in the output lists.  This allows for easy comparisons between the main synthetic control and placebo/leave-one-out synthetic controls.
#'
#' These anlayses require iterative fitting of synthetic controls, which MultiSynth performs.  Objects of class \code{MultiSynth} contain all of the relevent information for these anlayses.  Methods exist in \code{plot}, \code{summary}, \code{path.plot}, and \code{gaps.plot} for objects of class \code{MultiSynth}.    
#'
#'\code{MultiSynth} objects are further divided into three subclasses\code{\link{PlaceboMS}}, \code{\link{LOOunitsMS}}, and \code{\link{LOOcovariatesMS}}, depending on the type of analysis being carried out.   
#'
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{fitMultiSynth}}
#' @seealso \code{\link{plot,MultiSynth-method}}
#' @seealso \code{\link{summary,MultiSynth-method}}
#' @seealso \code{\link{path.plot,MultiSynth-method}}
#' @seealso \code{\link{gaps.plot,MultiSynth-method}}
#' @aliases MultiSynth-class initialize,MultiSynth-method, PlaceboMS-class initialize,PlaceboMS-method, LOOunitsMS-class LOOcovariatesMS-class
#' @rdname MultiSynth
#' @export
setClass(Class="MultiSynth", 
         representation = list(
           input = "list",
           preps = "list",
           fits = "list",
           treated = "character",
           treatment_time = "numeric",
           PreRMSPE = "numeric",
           PostRMSPE = "numeric",
           RMSPEratio = "numeric",
           CovBalances = "numeric",
           ATEs = "numeric"
         ),
         prototype = prototype(
           input = list(),
           preps = list(),
           fits = list(),
           treated = NA_character_,
           treatment_time = NA_real_,
           PreRMSPE = numeric(),
           PostRMSPE = numeric(),
           RMSPEratio = numeric(),
           CovBalances = numeric(),
           ATEs = numeric()
         )
)

#' @export
setMethod("initialize", "MultiSynth", 
          function(.Object, input = list(), preps = list(), fits = list(), treated = character(), treatment_time = numeric(), PreRMSPE = numeric(), PostRMSPE = numeric(), RMSPEratio = numeric(), CovBalances = numeric(), ATEs = numeric()
          ){ 
            .Object@input <- input
            .Object@preps <- preps
            .Object@fits <- fits
            .Object@treated <- treated
            .Object@treatment_time <- treatment_time
            .Object@PreRMSPE <- PreRMSPE
            .Object@PostRMSPE <- PostRMSPE
            .Object@RMSPEratio <- RMSPEratio
            .Object@CovBalances <- CovBalances
            .Object@ATEs <- ATEs
            .Object
          }
)

#' @export
setClass("PlaceboMS", contains = "MultiSynth",
         representation = list(
           p_value = "numeric"),
         prototype = prototype(
           p_value = numeric()
           )
         )

#' @export
setClass("LOOunitsMS", contains = "MultiSynth")

#' @export
setClass("LOOcovariatesMS", contains = "MultiSynth")

#' @export
setMethod("initialize", "PlaceboMS", 
          function(.Object, input = list(), preps = list(), fits = list(), treated = character(), treatment_time = numeric(), PreRMSPE = numeric(), PostRMSPE = numeric(), RMSPEratio = numeric(), CovBalances = numeric(), ATEs = numeric(), p_value = numeric()
          ){
            
            .Object@input <- input
            .Object@preps <- preps
            .Object@fits <- fits
            .Object@treated <- treated
            .Object@treatment_time <- treatment_time
            .Object@PreRMSPE <- PreRMSPE
            .Object@PostRMSPE <- PostRMSPE
            .Object@RMSPEratio <- RMSPEratio
            .Object@CovBalances <- CovBalances
            .Object@ATEs <- ATEs
            .Object@p_value <- p_value
            .Object
          }
)

