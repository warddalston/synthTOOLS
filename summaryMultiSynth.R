setMethod(f = "summary",
          signature = "PlaceboMS",
          def = function(object){
            
              Pre_print <- object@PreRMSPE
              Post_print <- object@PostRMSPE
              Ratio_print <- object@RMSPEratio
              Cov_print <- object@CovBalances
              ATEs_print <- object@ATEs
            
              Output <- cbind(Pre_print, Post_print, Ratio_print, Cov_print, ATEs_print)
              colnames(Output) <- c("Pre RMSPE", "Post RMSPE", "RMSPE Ratio", "Covariate Loss", "A.T.E")
              rownames(Output) <- names(Pre_print)
            
            cat("Placebo Analysis Summary for ", object@treated[1], "'s ", object@treatment_time, " treatment \n", sep = "")
            cat("Exact p-value for treated case: ", object@p_value, "\n", sep ="")
            print(round(Output, digits = 3))
            return(invisible(Output))
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
            
            Output <- cbind(Pre_print, Post_print, Ratio_print, Cov_print, ATEs_print)
            colnames(Output) <- c("Pre RMSPE", "Post RMSPE", "RMSPE Ratio", "Covariate Loss", "A.T.E")
            rownames(Output) <- names(Pre_print)
            
#             Z <- apply(Output,2, function(col) (col-mean(col))/sd(col) > 2)
#             
#             print_out <- ifelse(Z, paste(round(Output, 3), "*", sep =""), as.character(round(Output, 3)) )
#             
            cat("Leave-One-Out Units Analysis Summary for ", object@treated[1], "'s ", object@treatment_time, " treatment \n", sep = "")
            print(round(Output, 3))
            return(invisible(Output))
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
            
            Output <- cbind(Pre_print, Post_print, Ratio_print, Cov_print, ATEs_print)
            colnames(Output) <- c("Pre RMSPE", "Post RMSPE", "RMSPE Ratio", "Covariate Loss", "A.T.E")
            rownames(Output) <- names(Pre_print)
            
            cat("Leave-One-Out Covariates Analysis Summary for ", object@treated[1], "'s ", object@treatment_time, " treatment \n", sep = "")
            print(round(Output, digits = 3))
            return(invisible(Output))
          } #end function
) #end setMethod


summary(try1)
