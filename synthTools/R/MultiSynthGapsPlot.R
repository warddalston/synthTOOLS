#' Gaps plots for MultiSynth Objects
#' 
#' Plots the gaps in the trajectory of the outcome variable for the treated case and its synthetic control for each fit in a MultiSynth object.  The main analysis is plotted as a solid black line, and each of the placebo or leave-one-out fits is plotted as a light gray solid line.
#' 
#' @usage gaps.plot(synth.res = input,
#'  dataprep.res = NA,
#'  Ylab = c("Title"),
#'  Xlab = c("Time"),
#'  Main = c("Gaps: Treated - Synthetic"),
#'  tr.intake = NA,
#'  Ylim = NA, Z.plot = FALSE))
#'  
#' @param synth.res An object of class \code{\link{MultiSynth}}
#' @param dataprep.res NA.  Not used for MultiSynth gaps plotting, should be left as NA.
#' @param tr.intake NA. Not used for MultiSynth gaps plotting, should be left as NA.
#' @param Ylab Optional label for Y axis
#' @param Xlab Optional label for X axis 
#' @param Ylim Optional Y axis limits
#' @param Main Optional main title
#' @param Z.plot Logical. If TRUE only the pre-treatment period is plotted.
#' 
#' @details The user need only prvide an object of class \code{MultiSynth} as the arguement to synth.res for the function to work. Two other arguments, dataprep.res and tr.intake should be left as NA.  Other arguments are optional.  
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
#' @seealso \code{\link{path.plot,MultiSynth-method}}
#' @seealso \code{\link{gaps.plot}}
#'
#' @examples 
#' 
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
#'  ## Fit a multisynth object
#'  fitMultiSynth.out <- fitMultiSynth(dataprep.out, treatment_time = 1991)
#'  
#'  gaps.plot(fitMultiSynth.out)
#'  
#' @importFrom Synth gaps.plot 
#' @rdname gaps.plot.MultiSynth
#' @export
setMethod(f = "gaps.plot",
          signature = "MultiSynth",
          def = function(synth.res = NA, dataprep.res = NA, Ylab = c("Title"), Xlab = c("Time"), Main = c("Gaps: Treated - Synthetic"), tr.intake = NA, Ylim = NA, Z.plot = FALSE){
            
            #prep additional lines 
            plot_prep <- MultiSynthGapPlotPrep(synth.res@preps[2:length(synth.res@preps)], synth.res@fits[2:length(synth.res@fits)] )
            
            Max_gap <- max(abs(synth.res@input$Y1plot - (synth.res@input$Y0plot %*% synth.res@fits[[1]]$solution.w))) 
            
            y_lim_gaps <- if(any(sapply(plot_prep, function(list){ max(abs(list$y.info)) > Max_gap})) ){max(sapply(plot_prep, function(list){ max(abs(list$y.info) )} ) ) }  else { Max_gap} 
            
            y_lim_final <- if(is.na(Ylim[1])){c(-1*(y_lim_gaps + .3*y_lim_gaps), y_lim_gaps + .3*y_lim_gaps)} else {Ylim}
            
            #plot the basic thing
            gaps.plot(synth.res@fits[[1]], synth.res@input, synth.res@treatment_time, Ylab = Ylab, Xlab= Xlab, Ylim = y_lim_final, Main = Main, Z.plot = Z.plot)
            
            #plot new lines
            MultiSynthPlotLines(plot_prep)  
            
            return(invisible())
          }
          )
