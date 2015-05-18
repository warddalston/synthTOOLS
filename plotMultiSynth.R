setGeneric("RMSPEplot",
           def=function(x, ...){
             standardGeneric("RMSPEplot")
           }
)

setMethod(f = "plot",
          signature = "LOOunitsMS",
          def = function(x = input, y = NULL, quantity = "Ratios", main = NA, ...){
            
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
              
              print(names(sort(x@PreRMSPE, decreasing = TRUE)))
              print(is_treated)
              
              
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

setMethod(f = "plot",
          signature = "LOOcovariatesMS",
          def = function(x = input, y = NULL, quantity = "Ratios", main = NA, ...){
            
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

setMethod(f = "plot",
          signature = "PlaceboMS",
          def = function(x = input, y = NULL, quantity = "Ratios", main = NA, ...){
            
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
              
              print(names(sort(x@PreRMSPE, decreasing = TRUE)))
              print(is_treated)
              
              
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