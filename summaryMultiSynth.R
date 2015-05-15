setMethod(f = "summary",
          signature = "PlaceboMS",
          def = function(object){
            
              Pre_print <- object@PreRMSPE
              Post_print <- object@PostRMSPE
              Ratio_print <- object@RMSPEratio
              Cov_print <- object@CovBalances
              ATEs_print <- object@ATEs
            
            
            cat("Placebo Analysis for ", object@treated[1], "'s ", object@treatment_time, " treatment \n", sep = "")
            cat(" ****** Pre Treatment RMSPEs ****** \n")
            sapply(Pre_print, function(value){
              cat( names(Pre_print)[Pre_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            cat("\n ****** Post Treatment RMSPEs ****** \n")
            sapply(Post_print, function(value){
              cat( names(Post_print)[Post_print == value], ": ", round(value,3), "\n", sep = "")
            })
            cat("\n ****** Post-Pre RMSPE Ratios ****** \n")
            sapply(Ratio_print, function(value){
              cat( names(Ratio_print)[Ratio_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            cat("\n ****** Exact p-value: ", object@p_value, " ****** \n", sep = "")
            cat("\n ****** Covariate Balance ****** \n")
            sapply(Cov_print, function(value){
              cat( names(Cov_print)[Cov_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            cat("\n ****** Average Treatment Effects ****** \n")
            sapply(ATEs_print, function(value){
              cat( names(ATEs_print)[ATEs_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            return(invisible)
          } #end function
          ) #end setMethod

setMethod(f = "summary",
          signature = "LOOunitsMS",
          def = function(object){
            
            Pre_print <- object@PreRMSPE
            Post_print <- object@PostRMSPE
            Ratio_print <- object@RMSPEratio
            Cov_print <- object@CovBalances
            ATEs_print <- object@ATEs
            
            
            cat("Leave-One-Out Units Analysis for ", object@treated[1], "'s ", object@treatment_time, " treatment \n", sep = "")
            cat(" ****** Pre Treatment RMSPEs ****** \n")
            sapply(Pre_print, function(value){
              cat( names(Pre_print)[Pre_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            cat("\n ****** Post Treatment RMSPEs ****** \n")
            sapply(Post_print, function(value){
              cat( names(Post_print)[Post_print == value], ": ", round(value,3), "\n", sep = "")
            })
            cat("\n ****** Post-Pre RMSPE Ratios ****** \n")
            sapply(Ratio_print, function(value){
              cat( names(Ratio_print)[Ratio_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            cat("\n ****** Covariate Balance ****** \n")
            sapply(Cov_print, function(value){
              cat( names(Cov_print)[Cov_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            cat("\n ****** Average Treatment Effects ****** \n")
            sapply(ATEs_print, function(value){
              cat( names(ATEs_print)[ATEs_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            return(invisible())
          } #end function
) #end setMethod

setMethod(f = "summary",
          signature = "LOOcovariatesMS",
          def = function(object){
            
            Pre_print <- object@PreRMSPE
            Post_print <- object@PostRMSPE
            Ratio_print <- object@RMSPEratio
            Cov_print <- object@CovBalances
            ATEs_print <- object@ATEs
            
            
            cat("Leave-One-Out Covariates Analysis for ", object@treated[1], "'s ", object@treatment_time, " treatment \n", sep = "")
            cat(" ****** Pre Treatment RMSPEs ****** \n")
            sapply(Pre_print, function(value){
              cat( names(Pre_print)[Pre_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            cat("\n ****** Post Treatment RMSPEs ****** \n")
            sapply(Post_print, function(value){
              cat( names(Post_print)[Post_print == value], ": ", round(value,3), "\n", sep = "")
            })
            cat("\n ****** Post-Pre RMSPE Ratios ****** \n")
            sapply(Ratio_print, function(value){
              cat( names(Ratio_print)[Ratio_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            cat("\n ****** Covariate Balance ****** \n")
            sapply(Cov_print, function(value){
              cat( names(Cov_print)[Cov_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            cat("\n ****** Average Treatment Effects ****** \n")
            sapply(ATEs_print, function(value){
              cat( names(ATEs_print)[ATEs_print == value], ": ", round(value, 3), "\n", sep = "")
            })
            return(invisible())
          } #end function
) #end setMethod

summary(try1)
