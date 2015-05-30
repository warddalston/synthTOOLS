#' Prepare Data for a Placebo MultiSynth Analysis
#' 
#' This function creates the necessary sets of matrices for running a placebo MultiSynth analysis.  For each unit in the donor pool, it creates the X0, X1, Z0, Z1, Y0plot, and Y1plot matrices with the given unit assigned as the treated unit and the original treated unit assigned to the donor pool.  It also returns these six matrices for the original treated unit.  This function takes as input the output of \code{\link{dataprep}}.  This function is not intended to be called directly by the user, but rather is called indirectly by \code{\link{fitMultiSynth}}.
#' 
#' @param input The output from a call to \code{\link{dataprep}}
#' 
#' @details The output of this function is a named list, with each element being a list of the 6 matrices necessary for running \code{\link{synth}} and plotting its output.  If the user supplies character unit names in the call to dataprep used as the input, then the names of the output list will be these names.  Otherwise, unit numbers are the output list names.
#' 
#'   @return A list of lists, with each list containing the elements
#'   \itemize{
#'   \item{X0}{A matrix of predictor values for donor pool units}
#'   \item{X1}{A vector of predictor values for the treated unit}
#'   \item{Z0}{A matrix of pre-treatment outcome vaules for donor pool units}
#'   \item{Z1}{A vector of pre-treatment outcome values for the treated unit}
#'   \item{Y0plot}{A matrix of outcome values for the donor pool units}
#'   \item{Y1plot}{A vector of outcome values for the treated unit}
#'   }
#'   
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{fitMultiSynth}}
#' @seealso \code{\link{LOOunitsMSPrep}}
#' @seealso \code{\link{LOOcovariatesMSPrep}}
#' 
#' @rdname PlaceboMSPrep
#' @export
PlaceboMSPrep <- function(input){
    #create list of controls  
    units <- c(input$tag$treatment.identifier,input$tag$controls.identifier)
    
    #create the prep objects
    out <-   plyr::alply(.data = units, .margins = 1, .fun = function(x){
      
      #for the original, just return that
      if( x == input$tag$treatment.identifier){
        return(input[1:7])
      } 
      
      #for the others, create a placebo dataprep object
      temp <- list(
        X0 = cbind(input$X0[ ,!colnames(input$X0) == x ], input$X1),
        X1 = matrix(input$X0[ ,colnames(input$X0) == x ], 
                    dimnames = list(rownames(input$X0), as.character(x))),
        Z0 = cbind(input$Z0[ ,!colnames(input$Z0) == x ], input$Z1),
        Z1 = matrix(input$Z0[ ,colnames(input$Z0) == x ], 
                    dimnames = list(rownames(input$Z0), as.character(x))),
        Y0plot = cbind(input$Y0plot[ ,!colnames(input$Y0plot) == x ], input$Y1plot),
        Y1plot = matrix(input$Y0plot[ ,colnames(input$Y0plot) == x ], 
                        dimnames = list(rownames(input$Y0plot), as.character(x))),
        names.and.numbers = input$names.and.numbers
      ) #close temp list
      return(temp) #for the alply function
    })
    
    #give objects of this big list helpful names!
    names(out) <- as.character(input$names.and.numbers[,1])
    
    return(out)
  } #close function
 
#' Prepare Data for a Leave-One-Out Units MultiSynth Analysis
#' 
#' This function creates the necessary sets of matrices for running a leave-one-out units MultiSynth analysis.  For each unit in the donor pool, it creates the X0, X1, Z0, Z1, Y0plot, and Y1plot matrices with the given unit removed from the donor pool.  It also returns these six matrices for original case, with the full donor pool.  This function takes as input the output of \code{\link{dataprep}}.  This function is not intended to be called directly by the user, but rather is called indirectly by \code{\link{fitMultiSynth}}.
#' 
#' @param input The output from a call to \code{\link{dataprep}}
#' 
#' @details The output of this function is a named list, with each element being a list of the 6 matrices necessary for running \code{\link{synth}} and plotting its output.  If the user supplies character unit names in the call to dataprep used as the input, then the names of the output list will be these names.  Otherwise, unit numbers are the output list names.
#' 
#'   @return A list of lists, with each list containing the elements
#'   \itemize{
#'   \item{X0}{A matrix of predictor values for donor pool units}
#'   \item{X1}{A vector of predictor values for the treated unit}
#'   \item{Z0}{A matrix of pre-treatment outcome vaules for donor pool units}
#'   \item{Z1}{A vector of pre-treatment outcome values for the treated unit}
#'   \item{Y0plot}{A matrix of outcome values for the donor pool units}
#'   \item{Y1plot}{A vector of outcome values for the treated unit}
#'   }
#'   
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{fitMultiSynth}}
#' @seealso \code{\link{PlaceboMSPrep}}
#' @seealso \code{\link{LOOcovariatesMSPrep}}
#' 
#' @rdname LOOunitsMSPrep
#' @export
LOOunitsMSPrep  <- function(input){
    #create list of units  
    units <- c(input$tag$treatment.identifier,input$tag$controls.identifier)
    
    out <-   plyr::alply(.data = units, .margins = 1, .fun = function(x){
      
      #for the original, just return that
      if( x == input$tag$treatment.identifier){
        return(input[1:7])
      } 
      
      #for the others, create a placebo dataprep object
      temp <- list(
        X0 = input$X0[ ,!colnames(input$X0) == x ],
        X1 = input$X1,
        Z0 = input$Z0[ ,!colnames(input$Z0) == x ],
        Z1 = input$Z1,
        Y0plot = input$Y0plot[ ,!colnames(input$Y0plot) == x ],
        Y1plot = input$Y1plot,
        names.and.numbers = input$names.and.numbers[!input$names.and.numbers[,2] ==  x,]
      ) #close temp list
      return(temp) #for the sapply function
    })
    
    #give objects of this big list helpful names!
    names(out) <- c("Full Donor Pool", 
                    paste("Minus",as.character(input$names.and.numbers[-1,1]), sep = " "))
    return(out)
  } #end prep function 

#' Prepare Data for a Leave-One-Out Covariates MultiSynth Analysis
#' 
#' This function creates the necessary sets of matrices for running a leave-one-out covariates MultiSynth analysis.  For each covariate used in the main analysis, it creates an X0 and X1 matrix excluding a given covariate, copies the original Z0, Z1, Y0plot, Y1plot matrices, and creates a list containing these six matrices. It also returns the original set of matrices, with the full covariate set. This function takes as input the output of \code{\link{dataprep}}.  This function is not intended to be called directly by the user, but rather is called indirectly by \code{\link{fitMultiSynth}}.
#' 
#' @param input The output from a call to \code{\link{dataprep}}
#' 
#' @details The output of this function is a named list, with each element being a list of the 6 matrices necessary for running \code{\link{synth}} and plotting its output. The names of this list are "Full Covariate set" for the full set, and "Minus <<covariate name>>" for the leave-one-out cases.  
#' 
#'   @return A list of lists, with each list containing the elements
#'   \itemize{
#'   \item{X0}{A matrix of predictor values for donor pool units}
#'   \item{X1}{A vector of predictor values for the treated unit}
#'   \item{Z0}{A matrix of pre-treatment outcome vaules for donor pool units}
#'   \item{Z1}{A vector of pre-treatment outcome values for the treated unit}
#'   \item{Y0plot}{A matrix of outcome values for the donor pool units}
#'   \item{Y1plot}{A vector of outcome values for the treated unit}
#'   }
#'   
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{fitMultiSynth}}
#' @seealso \code{\link{PlaceboMSPrep}}
#' @seealso \code{\link{LOOunitsMSPrep}}
#' 
#' @rdname LOOcovariatesMSPrep
#' @export
LOOcovariatesMSPrep <- function(input){
    units <- c("FULL_SET", rownames(input$X0))
    
    out <-  plyr::alply(.data = units, .margins = 1, .fun = function(x){
      
      #for the original, just return that
      if( x == "FULL_SET"){
        return(input[1:7])
      } 
      
      #for the others, create a placebo dataprep object
      temp <- list(
        X0 = input$X0[ !rownames(input$X0) == x ,],
        X1 = matrix(input$X1[ !rownames(input$X1) == x ,], 
                    dimnames = list(rownames(input$X1)[!rownames(input$X1) == x], colnames(input$X1))),
        Z0 = input$Z0,
        Z1 = input$Z1,
        Y0plot = input$Y0plot,
        Y1plot = input$Y1plot,
        names.and.numbers = input$names.and.numbers
      ) #close temp list
      return(temp) #for the alply function
    })
    
    #give objects of this big list helpful names!
    names(out) <- c("Full Covariate Set", paste("Minus",units[-1], sep = " "))
  return(out)
} #close function 






