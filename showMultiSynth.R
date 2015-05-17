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
#             if(!is.null(object@input$tag$special.predictors)){
#               cat("\n Special Covariates: ")
#               sapply(object@input$tag$special.predictors, function(spec){
#                 
#                 if(length(spec[[2]]) != 1){
#                   specTime <- paste(spec[[2]][1], ":", spec[[2]][length(spec[[2]])], sep = "")
#                 } else(specTime <- spec[[2]])
#                 
#                 cat(spec[[1]], ", ", specTime, ", ", spec[[3]], "\n", sep = "")
#               })
#             }
            cat(" \n****** ****** \n")
            cat("Optimization Period:",min(object@input$tag$time.optimize.ssr), "to", max(object@input$tag$time.optimize.ssr), "\n" )
            cat( "Covariates Measurement Period:",min(object@input$tag$time.predictors.prior), "to", max(object@input$tag$time.predictors.prior), "\n" )
            return()
          } #end function
) #end setMethod
