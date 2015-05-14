setGeneric("RMSPEplot",
           def=function(input, ...){
             standardGeneric("RMSPEplot")
           }
)

setMethod(f = "RMSPEplot",
          signature = "LOOunitsMS",
          def = function(input, quantity = "Ratios",...){
            
            if(quantity == "Ratios"){
              
              #the main data to be plotted
              to_plot <- sort(input@RMSPEratio, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(input@RMSPEratio, decreasing = TRUE)) == "Full Donor Pool"
              
              # what to call it?
              Title <- paste("RMSPE Ratios for Leave-One-Out Units Analysis of ", input@treated[1], sep = "" )
              axLab <- "Post-Pre Treatment RMSPE Ratio"
              
            } else if(quantity == "Pre"){
              #the main data to be plotted
              to_plot <- sort(input@PreRMSPE, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(input@PreRMSPE, decreasing = TRUE)) == "Full Donor Pool"
              
              # what to call it?
              Title <- paste("Pre Treatment RMSPEs for Leave-One-Out Units Analysis of ", input@treated[1], sep = "" )
              axLab <- "Pre Treatment RMSPE Ratio"
            } else if(quantity == "Post"){
              #the main data to be plotted
              to_plot <- sort(input@PostRMSPE, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(input@PostRMSPE, decreasing = TRUE)) == "Full Donor Pool"
              
              # what to call it?
              Title <- paste("Post Treatment RMSPEs for Leave-One-Out Units Analysis of ", input@treated[1], sep = "" )
              axLab <- "Post Treatment RMSPE Ratio"
            } else if(quantity == "Cov"){
              #the main data to be plotted
              to_plot <- sort(input@CovBalances, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(input@CovBalances, decreasing = TRUE)) == "Full Donor Pool"
              
              # what to call it?
              Title <- paste("Covariate Balances for Leave-One-Out Units Analysis of ", input@treated[1], sep = "" )
              axLab <- "Covariate Balance"
            } else if(quantity == "ATE") {
              #the main data to be plotted
              to_plot <- sort(input@ATEs, decreasing = TRUE)
              
              #to highlight the main case
              is_treated <- names(sort(input@ATEs, decreasing = TRUE)) == "Full Donor Pool"
              
              # what to call it?
              Title <- paste("A.T.E.s  for Leave-One-Out Units Analysis of ", input@treated[1], sep = "" )
              axLab <- "A.T.E."
            }
            
              plot(x = to_plot, y = 1:length(to_plot), #basics: data by index
                   pch = 20, col = ifelse(is_treated,"red","black"), #make the case of interest red, others black
                   xlim = c(0, max(to_plot) + .05*(max(to_plot))), #axis just longer than the largest ratio
                   yaxs = "i", xaxs = "i", #for a closed box
                   ylim = c(0, length(to_plot)+1), #nice amount of space around everything
                   cex = 1.2, yaxt = "n", xaxt = "n", xlab = axLab, ylab = "", 
                   main = Title #fancy title.
              )
              #segments for aesthetic purposes
              segments(x0 = 0, x1 = to_plot, 
                       y0 = 1:length(to_plot), y1 = 1:length(to_plot),
                       lwd = 1.5, col = ifelse(is_treated,"red","black"))
              
              axis(2, at = 1:length(to_plot), labels = names(sort(input@RMSPEratio, decreasing = TRUE)), 
                   las = 1, cex.axis = .6, tick = F, lwd = 0, line = -.5)
              
              axis(1, tck = -.015, labels = NA)
              axis(1, lwd = 0, line = -.4)
            
            
          } #close function
          ) #close setMethod

RMSPEplot(try1, quantity = "Pre")
