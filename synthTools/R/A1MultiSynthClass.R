#' A MultiSynth (MS) Object
#' 
#' Objects of class \code{MultiSynth} are used in performing placebo and leave-one-out analyses for synthetic control studies, as described in Abadie, Diamond, Hainmueller (2010, 2015). MultiSynth objects are the output of the the main function of the \code{synthTOOLS} package, \code{\link{fitMultiSynth}}. They are created using the \code{initialize} function.      
#'
#' An object of the S4 class `MultiSynth' has the following slots:
#' \itemize{
#' \item \code{input} The output of a call to \code{\link{dataprep}}.  
#' \item \code{preps} A list with the X0, X1, Z0, Z1, Y0plot, and Y1plot for each of the placebo/leave-one-out cases and the treated/full donor pool/full covariate set case.
#' \item \code{fits} A list with the output of \code{synth} for each of the placebo/leave-one-out cases and the treated/full donor pool/full covariate set case.
#' \item \code{treated} A character vector of length two giving the name and number of the treated unit (if names are not supplied, the number is repeated)
#' \item \code{treatment_time} A scalar giving the time treatment is received
#' \item \code{PreRMSPE} A numeric vector giving the root mean square predictive error (RMSPE) in the pre-treatment period for each case. 
#' \item \code{PostRMSPE} A numeric vector giving the RMSPE in the post-treatment period for each case. 
#' \item \code{RMSPEratio} A numeric vector giving the ratio of post-treatment RMSPE to pre-treatment RMSPE for each case.
#' \item \code{CovBalances} A numeric vector giving the covariate loss for each case.
#' \item \code{ATEs} A numeric vector giving the average treatment effect (ATE) for each case.
#' \item \code{p_value} A scalar with the exact p-value of getting an RMSPE ratio as high as the treated unit, if choosing a unit to analyze at random. Only in \code{PlaceboMS} objects.  
#' }
#'
#'@details  
#'
#' MultiSynth objects can be used for in-space placebo analyses, leave-one-out units analyses, and leave-one-out covariates analyses. Though primarily created as the output of \code{\link{fitMultiSynth}}, they can also be created directly using \code{initialize}.
#'
#' These objects contain the dataprep for a synthetic control being analyzed, the dataprep matrices (X0, X1, Z0, Z1, Y0plot, Y1plot) for placebo or leave-one-out cases and the original/full case, the synthetic control fit for all cases, basic information on the synthetic control study, and statistics used to analyze a placebo/leave-one-out analysis. Note, MultiSynth refits the "main" analysis, and records this as the first element in the output lists.  This allows for easy comparisons between the main synthetic control and placebo/leave-one-out synthetic controls.  Methods exist for objects of class \code{MultiSynth} in \code{plot}, \code{summary}, \code{path.plot}, and \code{gaps.plot}.     
#'
#'\code{MultiSynth} objects are further divided into three subclasses\code{PlaceboMS}, \code{LOOunitsMS}, and \code{LOOcovariatesMS}, depending on the type of analysis being carried out.  There is very little difference between these three subclasses; they primarily impact printing and plot titles.  One important difference is the existence of the \code{p_value} slot in \code{PlaceboMS} objects.  This slot records the exact p-value of getting an RMSPE ratio as large as the treated cases' RMSPE ratio, if treatment is randomly assigned to any unit in the dataset.  
#'
#' Several functions exist for accessing the objects contained in a \code{MultiSynth} object.  \code{\link{getInput}} returns the output from dataprep used as the base of the analysis.  \code{\link{getPreps}} and \code{\link{getFits}} return the list of dataprep matrices and synth outputs, respectively.  \code{\link{getTreated}} and \code{\link{getTreatmentTime}} returns the name of the treated unit, and the time of treatment uptake.  For in depth analysis on a single case in a MultiSynth analysis, \code{\link{getCase}} returns the dataprep matrices and synth output for a single user specified case. Finally, \code{\link{getStats}} returns the matrix of RMSPE, ATE, and covariate balance statistics for the MultiSynth analysis.    
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
#' @seealso \code{\link{getInput}}, \code{\link{getPreps}}, \code{\link{getFits}}, \code{\link{getTreated}}, \code{\link{getTreatmentTime}}, \code{\link{getStats}}, \code{\link{getCase}}
#' @aliases MultiSynth-class MultiSynth
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

#' @aliases PlaceboMS-class
#' @rdname MultiSynth
#' @export
setClass("PlaceboMS", contains = "MultiSynth",
         representation = list(
           p_value = "numeric"),
         prototype = prototype(
           p_value = numeric()
           )
         )

#' @aliases LOOunitsMS-class
#' @rdname MultiSynth
#' @export
setClass("LOOunitsMS", contains = "MultiSynth")

#' @aliases LOOcovariatesMS-class
#' @rdname MultiSynth
#' @export
setClass("LOOcovariatesMS", contains = "MultiSynth")