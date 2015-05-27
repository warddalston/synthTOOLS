#' Show objects of class "MultiSynth"
#' 
#' Display the basic information about an object of class "MultiSynth".  This prints to the console the type of MultiSynth analysis, information about the treated unit, the donor pool, and covariate set, and the time periods used in fitting the analysis.  The function is invoked by automatic printing (when an object of class "MultiSynth" is sent to the console, or not assigned after a call to \code{\link{fitMultiSynth}}).
#' 
#' @usage show(object)
#' 
#' @param object An object of class MultiSynth.  Can be a placebo analysis or either type of leave-one-out analysis.  
#' 
#' @details There are methods of show defined for each of the three subclasses of MultiSynth.  The only differences between the show methods for these classes is the title of the output printed to the console.  
#' 
#' If the user does not provide character unit names in the \code{\link{dataprep}} output used in the call to \code{\link{fitMultiSynth}} then unit numbers are used in the place of unit names for this method.  
#' 
#' @return NULL
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{fitMultiSynth}}
#' @seealso \code{\link{MultiSynth}}
#' 
#' @rdname showMultiSynth
#' @export
setMethod(f = "show",
          signature = "PlaceboMS",
          def = function(object){
            
            cat("MultiSynth Placebo Analysis for ", ifelse(!is.na(object@input$tag$unit.names.variable), object@treated[1], paste("Unit", object@treated[1]) ), "'s ", object@treatment_time, " treatment \n", sep = "")
            cat(" ****** ******\n")
            cat("Donor Pool Units: \n")
            sapply(as.character(object@input$names.and.numbers[-1,1]), function(unit){
              if(which(as.character(object@input$names.and.numbers[-1,1]) == unit) %% 3 == 0 & which(as.character(object@input$names.and.numbers[-1,1]) == unit) != length(as.character(object@input$names.and.numbers[-1,1]))){
                cat(unit, "\n")
              } else( cat(unit, " "))
            })
            cat("\n ****** ******\n")
            cat("Covariates: \n")
            sapply(rownames(object@input$X0), function(cov){
              if(which(rownames(object@input$X0) == cov) %% 3 == 0 & which(rownames(object@input$X0) == cov) != length(rownames(object@input$X0)) ){
                cat(cov, "\n")
              } else( cat(cov, " "))
            })
            cat("\n ****** ****** \n")
            cat("Optimization Period:",min(object@input$tag$time.optimize.ssr), "to", max(object@input$tag$time.optimize.ssr), "\n" )
            cat( "Covariates Measurement Period:",min(object@input$tag$time.predictors.prior), "to", max(object@input$tag$time.predictors.prior), "\n" )
            return()
          } #end function
          ) #end setMethod

#' @rdname showMultiSynth
#' @export
setMethod(f = "show",
          signature = "LOOunitsMS",
          def = function(object){
            
            cat("MultiSynth Leave-One-Out Units Analysis for ", ifelse(!is.na(object@input$tag$unit.names.variable), object@treated[1], paste("Unit", object@treated[1]) ), "'s ", object@treatment_time, " treatment \n", sep = "")
            cat(" ****** ******\n")
            cat("Donor Pool Units: \n")
            sapply(as.character(object@input$names.and.numbers[-1,1]), function(unit){
              if(which(as.character(object@input$names.and.numbers[-1,1]) == unit) %% 3 == 0 & which(as.character(object@input$names.and.numbers[-1,1]) == unit) != length(as.character(object@input$names.and.numbers[-1,1]))){
                cat(unit, "\n")
              } else( cat(unit, " "))
            })
            cat("\n ****** ******\n")
            cat("Covariates: \n")
            sapply(rownames(object@input$X0), function(cov){
              if(which(rownames(object@input$X0) == cov) %% 3 == 0 & which(rownames(object@input$X0) == cov) != length(rownames(object@input$X0)) ){
                cat(cov, "\n")
              } else( cat(cov, " "))
            })
            cat("\n ****** ****** \n")
            cat("Optimization Period:",min(object@input$tag$time.optimize.ssr), "to", max(object@input$tag$time.optimize.ssr), "\n" )
            cat( "Covariates Measurement Period:",min(object@input$tag$time.predictors.prior), "to", max(object@input$tag$time.predictors.prior), "\n" )
            return()
          } #end function
) #end setMethod

#' @rdname showMultiSynth
#' @export
setMethod(f = "show",
          signature = "LOOcovariatesMS",
          def = function(object){
            
            cat("MultiSynth Leave-One-Out Covariatess Analysis for ", ifelse(!is.na(object@input$tag$unit.names.variable), object@treated[1], paste("Unit", object@treated[1]) ), "'s ", object@treatment_time, " treatment \n", sep = "")
            cat(" ****** ******\n")
            cat("Donor Pool Units: \n")
            sapply(as.character(object@input$names.and.numbers[-1,1]), function(unit){
              if(which(as.character(object@input$names.and.numbers[-1,1]) == unit) %% 3 == 0 & which(as.character(object@input$names.and.numbers[-1,1]) == unit) != length(as.character(object@input$names.and.numbers[-1,1]))){                cat(unit, "\n")
              } else( cat(unit, " "))
            })
            cat("\n ****** ******\n")
            cat("Covariates: \n")
            sapply(rownames(object@input$X0), function(cov){
              if(which(rownames(object@input$X0) == cov) %% 3 == 0 & which(rownames(object@input$X0) == cov) != length(rownames(object@input$X0)) ){
                cat(cov, "\n")
              } else( cat(cov, " "))
            })  
            cat(" \n****** ****** \n")
            cat("Optimization Period:",min(object@input$tag$time.optimize.ssr), "to", max(object@input$tag$time.optimize.ssr), "\n" )
            cat( "Covariates Measurement Period:",min(object@input$tag$time.predictors.prior), "to", max(object@input$tag$time.predictors.prior), "\n" )
            return()
          } #end function
) #end setMethod
