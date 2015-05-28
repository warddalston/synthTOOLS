#' Path plots for MultiSynth Objects
#' 
#' Plots the outcome trajectory of the outcome variable for the treated unit, the main synthetic control, and the place/leave-one-out synthetic controls created using \code{\link{fitMultiSynth}}.  The treated unit's trajectory is plotted as a solid line, the main synthetic control as a dashed line, and the MultiSynth controls as light gray solid lines.
#'  
#' @param synth.res An object of class \code{\link{MultiSynth}}
#' @param dataprep.res NA.  Not used for MultiSynth path plotting, should be left as NA.
#' @param tr.intake NA. Not used for MultiSynth path plotting, should be left as NA.
#' @param Ylab Optional label for Y axis
#' @param Xlab Optional label for X axis 
#' @param Ylim Optional Y axis limits
#' @param Legend Optional legend text.  If given as NA no legend is plotted. 
#' @param Legend.position Optional legend position.  See \code{\link{legend}} for details.
#' @param Main Optional main title
#' @param Z.plot Logical.  If TRUE only the pre-treatment period is plotted.
#' 
#' @details The user should give an object of class \code{\link{MultiSynth}} as the argument to synth.res, and leave dataprep.res and tr.intake as NA.  
#' 
#' This method for path.plot is generally designed to work with leave-one-out analayses.  It will work for placebo anlayses, but the interpretation of the plot is less clear. 
#' 
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{fitMultiSynth}}
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{plot,MultiSynth-method}}
#' @seealso \code{\link{summary,MultiSynth-method}}
#' @seealso \code{\link{gaps.plot,MultiSynth-method}}
#' @seealso \code{\link{path.plot}}
#' 
#' @examples
#' 
#' \dontrun{
#' ##Example: Hainmueller and Diamond's Toy panel dataset
#'
#'  #load data
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
#'  ##Fit a MultiSynth
#'  fitMultiSynth.out <- fitMultiSynth(dataprep.out, type = "units", treatment_time = 1991)
#'  
#' path.plot(fitMultiSynth.out)
#'
#'}
#'
#'@aliases path.plot,MultiSynth-Method
#' @importFrom Synth path.plot
#' @rdname path.plot.MultiSynth
#' @export
setMethod(f = "path.plot",
          signature = "MultiSynth",
          def = function(synth.res = NA, dataprep.res = NA, tr.intake = NA, Ylab = c("Y Axis"), Xlab = c("Time"), Ylim = NA, Legend = c("Treated", "Synthetic", "Alternative Synthetics"), Legend.position = c("topleft"), Main = NA, Z.plot = FALSE){
            
            #plot the basic thing
            path.plot(synth.res@fits[[1]], synth.res@input, synth.res@treatment_time, Ylab = Ylab, Xlab= Xlab, Ylim = Ylim, Legend = NA, Legend.position = Legend.position, Main = Main, Z.plot = Z.plot)
            
            #prep the additional lines
            plot_prep <- MultiSynthPathPlotPrep(synth.res@preps[2:length(synth.res@preps)], synth.res@fits[2:length(synth.res@fits)] )
            
            #plot new lines
            MultiSynthPlotLines(plot_prep) 
            
            if (sum(is.na(Legend)) == 0) {  
            legend(Legend.position, legend = Legend, lty = c(1,2,1), col = c("black", "black", "grey"), lwd = c(2, 2, 2), cex = 6/7)
            }
            
            return(invisible())
          } #end function
) #end setMethod
