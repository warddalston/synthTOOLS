#' Add Lines Representing Placebo/Leave-One-Out Anlayses to a Path or Gaps plot
#' 
#' This function plots the lines for the placebo/leave-one-out synthetic controls when \code{\link{path.plot}} or \code{\link{gaps.plot}} is called on a \code{\link{MultiSynth}} object.  It is not intended to be called directly by the user.  
#' 
#' 
#' @param MultiSynthPlotPrep_obj A list containing the x and y values fore each of the lines to be plotted
#' @param col Color to plot lines.  Defaults to gray.
#' @param lwd Width of plotted lines. Defaults to 3/4
#' @param cex Expansion parameter for plotting. Defaults to 1.
#' @param t Plotting type.  Defaults to "l" 
#' 
#' @details The function is called as part of \code{\link{path.plot,MultiSynth-method}} and \code{\link{gaps.plot,MultiSynth-method}}.  It is designed to take the output of \code{\link{MultiSynthPathPlotPrep}} or \code{\link{MultiSynthGapPlotPrep}} as its main input. If one wants to use a custom input object, the input should be a list with each element being a matrix with two columns. One of the columns, contianing x values, should be called "x.info", the other, with y values, called "y.info".    
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{path.plot,MultiSynth-method}}
#' @seealso \code{\link{gaps.plot,MultiSynth-method}}
#' @seealso \code{\link{MultiSynthGapPlotPrep}}
#' @seealso \code{\link{MultiSynthPathPlotPrep}}
#' 
#' @rdname MultiSynthPlotLines
#' @export
MultiSynthPlotLines <- function(MultiSynthPlotPrep_obj, col = "gray48", lwd = 3/4, cex = 1, t = "l"){
  lapply(MultiSynthPlotPrep_obj,function(controlCase){
    lines(controlCase$x.info, controlCase$y.info, t = t, col = col, lwd = lwd, cex = cex)
  }) # close lapply
} # close function 



#' Extract Values for Plotting the Gaps in Outcome for a MultiSynth Analysis
#' 
#' This function extracts the values necessary for plotting gaps in the outcome trajectory for placebo/leave-one-out synthetic controls from a list of dataprep matrices and a list of outputs from \code{\link{synth}}, such as are found in \code{\link{MultiSynth}} objects.  It is not intended to be called directly by the user, but is rather called as part of the the \code{\link{MultiSynth}} method for \code{\link{gaps.plot}}.  
#' 
#' @param MultiSynthPrep_obj A list the dataprep matrices for each placebo/leave-one-out analysis.
#' @param MultiSynth_obj A list containing the output from \code{synth} for each placebo/leave-one-out analysis. 
#' 
#' @details The function is called as part of \code{\link{gaps.plot,MultiSynth-method}}.  It is designed to take the the lists in the preps and fits slot of a \code{\link{MultiSynth}} object as its main inputs. If one wants to use a custom input, these should be lists containing outputs from \code{\link{dataprep}} and \code{\link{synth}}.    
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{gaps.plot,MultiSynth-method}}
#' @seealso \code{\link{MultiSynthPlotLines}}
#' @seealso \code{\link{MultiSynthPathPlotPrep}}
#' 
#' @rdname MultiSynthGapPlotPrep
#' @export
MultiSynthGapPlotPrep <- function(MultiSynthPrep_obj, MultiSynth_obj){ #the inputs are lists.  
  
  mapply(function(prep, synth){
    
    gap <- prep$Y1plot - (prep$Y0plot %*% synth$solution.w)
    x.info <- as.numeric(rownames(prep$Y0plot))
    
    return(list(y.info = gap, x.info = x.info))
    
  }, MultiSynthPrep_obj, MultiSynth_obj, SIMPLIFY=FALSE)
} # close function

#' Extract Values for Plotting the Outcome Trajectories for a MultiSynth Analysis
#' 
#' This function extracts the values necessary for plotting the outcome trajectory for placebo/leave-one-out synthetic controls from a list of dataprep matrices and a list of outputs from \code{\link{synth}}, such as are found in \code{\link{MultiSynth}} objects.  It is not intended to be called directly by the user, but is rather called as part of the the \code{\link{MultiSynth}} methods for \code{\link{path.plot}}.  
#' 
#' @param MultiSynthPrep_obj A list the dataprep matrices for each placebo/leave-one-out analysis.
#' @param MultiSynth_obj A list containing the output from \code{synth} for each placebo/leave-one-out analysis. 
#' 
#' @details The function is called as part of \code{\link{path.plot,MultiSynth-method}}.  It is designed to take the the lists in the preps and fits slot of a \code{\link{MultiSynth}} object as its main input. If one wants to use a custom input, these should be lists containing outputs from \code{\link{dataprep}} and \code{\link{synth}}.    
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{path.plot,MultiSynth-method}}
#' @seealso \code{\link{MultiSynthPlotLines}}
#' @seealso \code{\link{MultiSynthGapPlotPrep}}
#' 
#' @rdname MultiSynthPathPlotPrep
#' @export
MultiSynthPathPlotPrep <- function(MultiSynthPrep_obj, MultiSynth_obj){
  
  if(length(MultiSynthPrep_obj) != length(MultiSynth_obj)){
    stop("There should be the same number of Preps and fits! Something's gone wrong")
  }
  
  mapply(function(prep, synth){
    
    path <- prep$Y0plot %*% synth$solution.w
    x.info <- as.numeric(rownames(prep$Y0plot))
    return(list(y.info = path, x.info = x.info))
    
  }, MultiSynthPrep_obj, MultiSynth_obj, SIMPLIFY=FALSE)
}