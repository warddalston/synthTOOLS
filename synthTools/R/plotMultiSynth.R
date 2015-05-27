#' Plot MultiSynth objects
#' 
#' This function plots the distribution of values for the statistics calculated in a MultiSynth analysis.  It can plot five quantities: pretreatment RMSPEs, posttreatment RMSPEs, post-to-pre-treatment RMSPE ratios, ATEs, and covariate loss.  The user chooses which quantity to plot through the arguement quantity.  The function first sorts the quantity of interest, and then plots on the x-axis the values of the quantity for each case in the MultiSynth analysis.  Axis labels correspond to the names of the placebo/leave-one-out cases.  The actual treated unit or the analysis with the full donor pool/covariate set is plotted in red.  
#' 
#' @usage plot(x = input, quantity = "Ratios", main = NA, ...)
#' 
#' @param x A MultiSynth object
#' @param quantity The quantity to plot.  Can be "Ratios" for RMSPE ratio, "Pre" for pretreatment RMSPE, "Post" for posttreatment RMSPE, "ATE" for ATEs, or "Cov" for covariate balance.  Defaults to "Ratios".  
#' @param main Optional main title. Defaults to "<<Quantity to plot>> for <<type of MultiSynth>> Analysis of <<treated unit name>>". 
#' @param ... further arguments passed on to the default method of plot
#' 
#' @details The three different subclasses of MultiSynth objects (PlaceboMS, LOOunitsMS, and LOOcovariatesMS) each have their own method for plot.  The only difference between these three methods, however, is the text printed by default for the main plot title.  When unit names or covariate names are very long, they may not fit in the plotting space.  This can be corrected by adjusting the plotting margins via \code{\link{par}}.  Additionally, the names of the cases can be adjusted manually through a call to \code{names} where the argument is the slot of a MultiSynth object containing the quantity to be plotted.  Adjusting names should be done with caution; if the name for the treated/complete case is changed, it will not be plotted in red.  
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
#'  plot(fitMultiSynth.out)
#'  
#'  plot(fitMultiSynth.out, quantity = "ATE")
#'  
#'  @rdname plot,MultiSynth-method
#'  @export
setMethod(f = "plot",
          signature = "LOOunitsMS",
          def = function(x = input, y = NULL, quantity = "Ratios", main = NA, ...){
            
            if(!quantity %in% c("Ratios", "Pre", "Post", "ATE", "Cov")){
              stop("Please enter a valid argument for quantity!")
            }
            
            if(quantity == "Ratios"){
              
              #the main data to be plotted
              to_plot <- sort(x@RMSPEratio, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@RMSPEratio, decreasing = TRUE)) == "Full Donor Pool"
              
              # what to call it?
              Title <- paste("RMSPE Ratios for Leave-One-Out Units Analysis of ", x@treated[1], sep = "" )
              axLab <- "Post-Pre Treatment RMSPE Ratio"
              plot_labels <- names(sort(x@RMSPEratio, decreasing = TRUE))
              
              
            } else if(quantity == "Pre"){
              #the main data to be plotted
              to_plot <- sort(x@PreRMSPE, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@PreRMSPE, decreasing = TRUE)) == "Full Donor Pool"
                            
              # what to call it?
              Title <- paste("Pre Treatment RMSPEs for Leave-One-Out Units Analysis of ", x@treated[1], sep = "" )
              axLab <- "Pre Treatment RMSPE Ratio"
              plot_labels <- names(sort(x@PreRMSPE, decreasing = TRUE))
              
            } else if(quantity == "Post"){
              #the main data to be plotted
              to_plot <- sort(x@PostRMSPE, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@PostRMSPE, decreasing = TRUE)) == "Full Donor Pool"
              
              # what to call it?
              Title <- paste("Post Treatment RMSPEs for Leave-One-Out Units Analysis of ", x@treated[1], sep = "" )
              axLab <- "Post Treatment RMSPE Ratio"
              plot_labels <- names(sort(x@PostRMSPE, decreasing = TRUE))
              
            } else if(quantity == "Cov"){
              #the main data to be plotted
              to_plot <- sort(x@CovBalances, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@CovBalances, decreasing = TRUE)) == "Full Donor Pool"
              
              # what to call it?
              Title <- paste("Covariate Balances for Leave-One-Out Units Analysis of ", x@treated[1], sep = "" )
              axLab <- "Covariate Balance"
              plot_labels <- names(sort(x@CovBalances, decreasing = TRUE))
              
              
            } else if(quantity == "ATE") {
              #the main data to be plotted
              to_plot <- sort(x@ATEs, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@ATEs, decreasing = TRUE)) == "Full Donor Pool"
              
              # what to call it?
              Title <- paste("A.T.E.s  for Leave-One-Out Units Analysis of ", x@treated[1], sep = "" )
              axLab <- "A.T.E."
              plot_labels <- names(sort(x@ATEs, decreasing = TRUE))
              
            }
            
            main_title <- ifelse(is.na(main), Title, main)
            
              plot(x = to_plot, y = 1:length(to_plot), #basics: data by index
                   pch = 20, col = ifelse(is_treated,"red","black"), #make the case of interest red, others black
                   xlim = c(
                     ifelse(quantity == "ATE" & any(x@ATEs < 0), min(to_plot) + .05*min(to_plot),0),
                     ifelse(quantity == "ATE" & all(x@ATEs < 0), 0, max(to_plot) + .05*max(to_plot)) ), #axis just longer than the largest ratio
                   yaxs = "i", xaxs = "i", #for a closed box
                   ylim = c(0, length(to_plot)+1), #nice amount of space around everything
                   cex = 1.2, yaxt = "n", xaxt = "n", xlab = axLab, ylab = "", 
                   main =  main_title, ... #fancy title.
              )
              #segments for aesthetic purposes
              segments(x0 = 0, x1 = to_plot, 
                       y0 = 1:length(to_plot), y1 = 1:length(to_plot),
                       lwd = 1.5, col = ifelse(is_treated,"red","black"))
              
              axis(2, at = 1:length(to_plot), labels = plot_labels, 
                   las = 1, cex.axis = .6, tick = F, lwd = 0, line = -.5)
              
              axis(1, tck = -.015, labels = NA)
              axis(1, lwd = 0, line = -.4)
            
            #plots a line at zero to make ATE plots nicer
            if(quantity == "ATE" & any(x@ATEs > 0) & any(x@ATEs < 0)){
              segments(x0 = 0, x1 = 0, y0 = 1, y1 = length(to_plot))
            }
          } #close function
          ) #close setMethod

#'  @rdname plot,MultiSynth-method
#'  @export
setMethod(f = "plot",
          signature = "LOOcovariatesMS",
          def = function(x = input, y = NULL, quantity = "Ratios", main = NA, ...){
            
            if(!quantity %in% c("Ratios", "Pre", "Post", "ATE", "Cov")){
              stop("Please enter a valid argument for quantity!")
            }
            
            
            if(quantity == "Ratios"){
              
              #the main data to be plotted
              to_plot <- sort(x@RMSPEratio, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@RMSPEratio, decreasing = TRUE)) == "Full Covariate Set"
              
              # what to call it?
              Title <- paste("RMSPE Ratios for Leave-One-Out Covariates Analysis of ", x@treated[1], sep = "" )
              axLab <- "Post-Pre Treatment RMSPE Ratio"
              plot_labels <- names(sort(x@RMSPEratio, decreasing = TRUE))
              
              
            } else if(quantity == "Pre"){
              #the main data to be plotted
              to_plot <- sort(x@PreRMSPE, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@PreRMSPE, decreasing = TRUE)) == "Full Covariate Set"
              
              # what to call it?
              Title <- paste("Pre Treatment RMSPEs for Leave-One-Out Covariates Analysis of ", x@treated[1], sep = "" )
              axLab <- "Pre Treatment RMSPE Ratio"
              plot_labels <- names(sort(x@PreRMSPE, decreasing = TRUE))
              
            } else if(quantity == "Post"){
              #the main data to be plotted
              to_plot <- sort(x@PostRMSPE, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@PostRMSPE, decreasing = TRUE)) == "Full Covariate Set"
              
              # what to call it?
              Title <- paste("Post Treatment RMSPEs for Leave-One-Out Covariates Analysis of ", x@treated[1], sep = "" )
              axLab <- "Post Treatment RMSPE Ratio"
              plot_labels <- names(sort(x@PostRMSPE, decreasing = TRUE))
              
            } else if(quantity == "Cov"){
              #the main data to be plotted
              to_plot <- sort(x@CovBalances, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@CovBalances, decreasing = TRUE)) == "Full Covariate Set"
              
              # what to call it?
              Title <- paste("Covariate Balances for Leave-One-Out Covariates Analysis of ", x@treated[1], sep = "" )
              axLab <- "Covariate Balance"
              plot_labels <- names(sort(x@CovBalances, decreasing = TRUE))
              
              
            } else if(quantity == "ATE") {
              #the main data to be plotted
              to_plot <- sort(x@ATEs, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@ATEs, decreasing = TRUE)) == "Full Covariate Set"
              
              # what to call it?
              Title <- paste("A.T.E.s  for Leave-One-Out Covariates Analysis of ", x@treated[1], sep = "" )
              axLab <- "A.T.E."
              plot_labels <- names(sort(x@ATEs, decreasing = TRUE))
              
            }
            
            main_title <- ifelse(is.na(main), Title, main)
            
            plot(x = to_plot, y = 1:length(to_plot), #basics: data by index
                 pch = 20, col = ifelse(is_treated,"red","black"), #make the case of interest red, others black
                 xlim = c(ifelse(quantity == "ATE" & any(x@ATEs < 0), min(to_plot) + .05*min(to_plot),0), ifelse(quantity == "ATE" & all(x@ATEs < 0), 0, max(to_plot) + .05*max(to_plot)) ), #axis just longer than the largest ratio
                 yaxs = "i", xaxs = "i", #for a closed box
                 ylim = c(0, length(to_plot)+1), #nice amount of space around everything
                 cex = 1.2, yaxt = "n", xaxt = "n", xlab = axLab, ylab = "", 
                 main = main_title, ... #fancy title.
            )
            #segments for aesthetic purposes
            segments(x0 = 0, x1 = to_plot, 
                     y0 = 1:length(to_plot), y1 = 1:length(to_plot),
                     lwd = 1.5, col = ifelse(is_treated,"red","black"))
            
            axis(2, at = 1:length(to_plot), labels = plot_labels, 
                 las = 1, cex.axis = .6, tick = F, lwd = 0, line = -.5)
            
            axis(1, tck = -.015, labels = NA)
            axis(1, lwd = 0, line = -.4)
           
            #plots a line at zero to make ATE plots nicer.
            if(quantity == "ATE" & any(x@ATEs > 0) & any(x@ATEs < 0)){
              segments(x0 = 0, x1 = 0, y0 = 1, y1 = length(to_plot))
            }
            
          } #close function
) #close setMethod

#'  @rdname plot,MultiSynth-method
#'  @export
setMethod(f = "plot",
          signature = "PlaceboMS",
          def = function(x = input, y = NULL, quantity = "Ratios", main = NA, ...){
            
            if(!quantity %in% c("Ratios", "Pre", "Post", "ATE", "Cov")){
              stop("Please enter a valid argument for quantity!")
            }
            
            if(quantity == "Ratios"){
              
              #the main data to be plotted
              to_plot <- sort(x@RMSPEratio, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@RMSPEratio, decreasing = TRUE)) == x@treated[1]
              
              # what to call it?
              Title <- paste("RMSPE Ratios for Placebo Analysis of ", x@treated[1], sep = "" )
              axLab <- "Post-Pre Treatment RMSPE Ratio"
              plot_labels <- names(sort(x@RMSPEratio, decreasing = TRUE))
              
              
            } else if(quantity == "Pre"){
              #the main data to be plotted
              to_plot <- sort(x@PreRMSPE, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@PreRMSPE, decreasing = TRUE)) == x@treated[1]
              
              # what to call it?
              Title <- paste("Pre Treatment RMSPEs for Place Analysis of ", x@treated[1], sep = "" )
              axLab <- "Pre Treatment RMSPE Ratio"
              plot_labels <- names(sort(x@PreRMSPE, decreasing = TRUE))
              
            } else if(quantity == "Post"){
              #the main data to be plotted
              to_plot <- sort(x@PostRMSPE, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@PostRMSPE, decreasing = TRUE)) == x@treated[1]
              
              # what to call it?
              Title <- paste("Post Treatment RMSPEs for Placebo Analysis of ", x@treated[1], sep = "" )
              axLab <- "Post Treatment RMSPE Ratio"
              plot_labels <- names(sort(x@PostRMSPE, decreasing = TRUE))
              
            } else if(quantity == "Cov"){
              #the main data to be plotted
              to_plot <- sort(x@CovBalances, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@CovBalances, decreasing = TRUE)) == x@treated[1]
              
              # what to call it?
              Title <- paste("Covariate Balances for Placebo Analysis of ", x@treated[1], sep = "" )
              axLab <- "Covariate Balance"
              plot_labels <- names(sort(x@CovBalances, decreasing = TRUE))
              
              
            } else if(quantity == "ATE") {
              #the main data to be plotted
              to_plot <- sort(x@ATEs, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(x@ATEs, decreasing = TRUE)) == x@treated[1]
              
              # what to call it?
              Title <- paste("A.T.E.s  for Placebo Analysis of ", x@treated[1], sep = "" )
              axLab <- "A.T.E."
              plot_labels <- names(sort(x@ATEs, decreasing = TRUE))
              
            }
            
            main_title <- ifelse(is.na(main), Title, main)
            
            plot(x = to_plot, y = 1:length(to_plot), #basics: data by index
                 pch = 20, col = ifelse(is_treated, "red", "black"), #make the case of interest red, others black
                 xlim = c(ifelse(quantity == "ATE" & any(x@ATEs < 0), min(to_plot) + .05*min(to_plot),0), ifelse(quantity == "ATE" & all(x@ATEs < 0), 0, max(to_plot) + .05*max(to_plot)) ), #axis just longer than the largest ratio
                 yaxs = "i", xaxs = "i", #for a closed box
                 ylim = c(0, length(to_plot)+1), #nice amount of space around everything
                 cex = 1.2, yaxt = "n", xaxt = "n", xlab = axLab, ylab = "", 
                 main =  main_title, ... #fancy title.
            )
            #segments for aesthetic purposes
            segments(x0 = 0, x1 = to_plot, 
                     y0 = 1:length(to_plot), y1 = 1:length(to_plot),
                     lwd = 1.5, col = ifelse(is_treated,"red","black"))
                        
            axis(2, at = 1:length(to_plot), labels = plot_labels, 
                 las = 1, cex.axis = .6, tick = F, lwd = 0, line = -.5)
            
            axis(1, tck = -.015, labels = NA)
            axis(1, lwd = 0, line = -.4)
            
            #plots a line at zero to make ATE plots look nice
            if(quantity == "ATE" & any(x@ATEs > 0) & any(x@ATEs < 0)){
              segments(x0 = 0, x1 = 0, y0 = 1, y1 = length(to_plot))
            }
            
          } #close function
) #close setMethod