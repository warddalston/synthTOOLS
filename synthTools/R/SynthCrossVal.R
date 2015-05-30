#' Fit a Synthetic Control with Covariate Weights Chosen by Cross Validation
#' 
#' Implements a cross validation method for choosing weights on covariates when fitting a synthetic control, as described by Abadie, Diamond, Hainmueller (2015).  In this cross validation method, the pretreatment period is divided into a training period and a validation period. Then, covariate data from the pretreatment period is used to optimize the fit of a synthetic control over the validation period.  This produces a set of weights on covariates.  These weights are then used with covariate data from the validation period to optimize the fit of the synthetic control over the entire pretreatment period.
#'  
#'  @usage SynthCrossVal(foo = NULL,
#' predictors = NULL,
#'  predictors.op = "mean",
#' special.predictors = NULL, 
#' dependent = NULL,
#' unit.variable = NULL, 
#' time.variable = NULL,
#' treatment.identifier = NULL,
#'  controls.identifier = NULL,
#' time.training = NULL,
#'  time.validation = NULL,
#' time.optimize.final = NULL,
#'  time.plot = time.optimize.final,
#' unit.names.variable = NA, ...)
#' 
#' @param foo The dataframe with panel data
#' @param predictors A vector of column numbers of column-name character strings that identifies the predictors' columns.  All predictors must be numeric. 
#' @param predictors.op A character string identifying the method (operator) to be used on the predictors.  Default is "mean".  
#' @param special.predictors A list object identifying additional numeric predictors and their assocaited pre-treatment years and operators.  
#' @param dependent A scalar identifying the column number or a string identifying the column name correspoding to the numeric dependent (outcome) variable. 
#' @param unit.variable A scalar identifying the column number or a string identifying the column name correspoding to the column containing unit numbers.  Must be numeric. 
#' @param time.variable A scalar identifying the column number or a string identifying the column name corresponding to the period (time) data.  This must be numeric.  
#' @param treatment.identifier A scalar identifying the "unit.variable" number or a string giving the "unit.name" of the treated unit.  If a string is given, then unit.names.variable cannot be NA. 
#' @param controls.identifier A numeric vector identifying the "unit.variable" numbers of a string vector giving the "unit.names" of the donor pool units.  If a string vector is given, then unit.names.variable cannot be NA. 
#' @param time.training A numeric vector identifying the pretreatment periods over which predictors should be measured for the fitting the training synthetic control.  
#' @param time.validation A numeric vector identifying the pretreatment periods over which the loss function should be minimized for fitting the training control and the periods over which predictors should be measured for fitting the final synthetic control. 
#' @param time.optimize.final A numeric vector identifying the pretreatment periods over which the loss function should be minimized for fitting the final synthetic control.
#' @param time.plot A numeric vector identifying the periods over which results are to be plotted with \code{\link{gaps.plot}} and \code{\link{path.plot}}. Defaults to the periods given in time.optimize.final
#' @param unit.names.variable An optional scalar or column-name character string identifying the column with the names of the units.  This must be a character vector.
#' @param ... futher arguments to be passed to \code{synth}
#' 
#' @details  According to Abadie, Diamond, and Hainmueller (2015), this procedure minimizes out of sample prediction errors.  However, they also note that it tends to produce results similar to the standard fitting procedure, as implemented in \code{\link{synth}}  
#' 
#'  This function implements this cross validation method by iteratively creating dataprep objects and fitting synthetic controls.  It fits two synthetic controls: the training synthetic control and the final synthetic control. 
#'  
#'   The use of the function is similar to \code{\link{dataprep}} except that the user must now specifiy which time periods comprising the training and validation periods, and the optimization period for the final synthetic control. 
#'   
#' For more on the input required for predictors.op and special.predictors, see \code{\link{dataprep}}.  The code does not currently allow for cross-validation of special predictors; the same values are used in both the training synthetic control and final synthetic control for these predictors.
#' 
#'   The output of the function is a list containing the output of the two calls to dataprep and synth.  The third (final dataprep output) and fourth (final synth output) elements of the list returned by \code{SynthCrossVal} can be used to summarize the results of a synthetic control fit by cross validation in other functions (e.g. \code{\link{gaps.plot}}, \code{\link{path.plot}}, \code{\link{synth.tab}}). 
#' 
#' @return A list containing four elements:
#' \itemize{
#' \item \code{dataprep.training} {The output of the call to dataprep for the training control}
#' \item \code{synth.training} {The output of the call to synth for the training control}
#' \item \code{dataprep.final} {The output of the call to dataprep for the final control}
#' \item \code{synth.final} {The output of the call to synth for the final control}
#' }
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{dataprep}}
#' @seealso \code{\link{synth}}
#' 
#' @examples 
#' 
#' ##Example: Hainmueller and Diamond's Toy panel dataset
#'
#' #load data
#' data(synth.data)
#'
#'  synthCV.out <- 
#'  SynthCrossVal(
#'    foo = synth.data,
#'    predictors = c("X1", "X2", "X3"),
#'    predictors.op = "mean",
#'    dependent = "Y",
#'    unit.variable = "unit.num",
#'    time.variable = "year",,
#'    treatment.identifier = 7,
#'    controls.identifier = c(29, 2, 13, 17, 32, 38),
#'    time.training = c(1984:1985),
#'    time.validation = c(1986:1990),
#'    time.optimize.final = c(1984:1990),
#'    unit.names.variable = "name",
#'    time.plot = 1984:1996
#'  )
#'  
#'  synth.tab(synthCV.out$synth.final, synthCV.out$dataprep.final)
#'  
#'  @rdname SynthCrossVal
#'  @export
SynthCrossVal <- function(foo = NULL,
                          predictors = NULL,
                          predictors.op = "mean",
                          special.predictors = NULL,
                          dependent = NULL,
                          unit.variable = NULL,
                          time.variable = NULL,
                          treatment.identifier = NULL,
                          controls.identifier = NULL,
                          time.training = NULL,
                          time.validation = NULL,
                          time.optimize.final = NULL,
                          time.plot = time.optimize.final,
                          unit.names.variable = NA,
                          ...
                          ){
  
  dataprep.training <- dataprep(foo = foo,
                                predictors = predictors,
                                predictors.op = predictors.op,
                                special.predictors = special.predictors,
                                dependent = dependent,
                                unit.variable = unit.variable,
                                time.variable = time.variable,
                                treatment.identifier = treatment.identifier,
                                controls.identifier = controls.identifier,
                                time.predictors.prior = time.training,
                                time.optimize.ssr = time.validation,
                                time.plot = time.plot,
                                unit.names.variable = unit.names.variable
                                )
  
  synth.training <- synth(dataprep.training, ...)
  
  dataprep.final <- dataprep(foo = foo,
                             predictors = predictors,
                             predictors.op = predictors.op,
                             special.predictors = special.predictors,
                             dependent = dependent,
                             unit.variable = unit.variable,
                             time.variable = time.variable,
                             treatment.identifier = treatment.identifier,
                             controls.identifier = controls.identifier,
                             time.predictors.prior = time.validation,
                             time.optimize.ssr = time.optimize.final,
                             time.plot = time.plot,
                             unit.names.variable = unit.names.variable)
  
  synth.final <- synth(dataprep.final, custom.v = as.numeric(synth.training$solution.v), ...)
  
  return(list(dataprep.training = dataprep.training, synth.training = synth.training, dataprep.final = dataprep.final, synth.final = synth.final))
}