#' Summary of a MultiSynth Analysis
#' 
#' This method provides a summary of the results of a MultiSynth analysis.  It prints to the console the values of pre and post treatment RMSPEs, their ratio, ATEs, and covariate loss statistics for each case fitted in a MultiSynth analysis. Together, these quantities allow the user to determine the quality of fit, robustness, and significance of a synthetic control analysis.  For details, see Abadie, Diamond, Hainmueller (2010, 2015).  Furthermore, for placebo analysis, the summary method for MultiSynth prints the p-value of obtaining an RMSPE ratio as large as the treated unit when treatment is randomly assigned to any unit in the donor pool. A matrix containing all of these statistics is also returned by the function.  
#' 
#' @usage summary(object, digits = 3)
#' 
#' @param object An object of class MultiSynth
#' @param digits The number of digits to print.  Defaults to 3.
#' 
#' @details Methods for summary are defined for each of the three MultiSynth subclasses.  However, there are only very minor differences between the three methods.  First, the title of the output printed varies depending on the type of MultiSynth analysis.  Second, for placebo anlayses, the p-value for the RMSPE ratio is printed to the console.  Otherwise, the three methods are exactly the same.
#' 
#' @return A matrix containing all of the statistics printed to the console.  
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @example 
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
#'  summary(fitMultiSynth.out) 
#'
#' @rdname summary,MultiSynth-method
#' @export
setMethod(f = "summary",
          signature = "PlaceboMS",
          def = function(object, digits = 3){
            
              Pre_print <- object@PreRMSPE
              Post_print <- object@PostRMSPE
              Ratio_print <- object@RMSPEratio
              Cov_print <- object@CovBalances
              ATEs_print <- object@ATEs
            
              Output <- cbind(Pre_print, Post_print, Ratio_print, Cov_print, ATEs_print)
              colnames(Output) <- c("Pre RMSPE", "Post RMSPE", "RMSPE Ratio", "Covariate Loss", "A.T.E")
              rownames(Output) <- names(Pre_print)
            
            cat("Placebo Analysis Summary for ", object@treated[1], "'s ", object@treatment_time, " treatment \n", sep = "")
            cat("Exact p-value for treated case: ", round(object@p_value, digits), "\n", sep ="")
            print(round(Output, digits))
            return(invisible(Output))
          } #end function
          ) #end setMethod

#' @rdname summary,MultiSynth-method
#' @export
setMethod(f = "summary",
          signature = "LOOunitsMS",
          def = function(object, digits = 3){
            
            Pre_print <- object@PreRMSPE
            Post_print <- object@PostRMSPE
            Ratio_print <- object@RMSPEratio
            Cov_print <- object@CovBalances
            ATEs_print <- object@ATEs
            
            Output <- cbind(Pre_print, Post_print, Ratio_print, Cov_print, ATEs_print)
            colnames(Output) <- c("Pre RMSPE", "Post RMSPE", "RMSPE Ratio", "Covariate Loss", "A.T.E")
            rownames(Output) <- names(Pre_print)
            
            cat("Leave-One-Out Units Analysis Summary for ", object@treated[1], "'s ", object@treatment_time, " treatment \n", sep = "")
            print(round(Output, digits))
            return(invisible(Output))
          } #end function
) #end setMethod

#' @rdname summary,MultiSynth-method
#' @export
setMethod(f = "summary",
          signature = "LOOcovariatesMS",
          def = function(object, digits = 3){
            
            Pre_print <- object@PreRMSPE
            Post_print <- object@PostRMSPE
            Ratio_print <- object@RMSPEratio
            Cov_print <- object@CovBalances
            ATEs_print <- object@ATEs
            
            Output <- cbind(Pre_print, Post_print, Ratio_print, Cov_print, ATEs_print)
            colnames(Output) <- c("Pre RMSPE", "Post RMSPE", "RMSPE Ratio", "Covariate Loss", "A.T.E")
            rownames(Output) <- names(Pre_print)
            
            cat("Leave-One-Out Covariates Analysis Summary for ", object@treated[1], "'s ", object@treatment_time, " treatment \n", sep = "")
            print(round(Output, digits))
            return(invisible(Output))
          } #end function
) #end setMethod