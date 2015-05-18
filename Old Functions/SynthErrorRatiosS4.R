setGeneric(name = "SynthErrorRatios",
           def=function(prep, fit, treatment_time, ...)
           {standardGeneric("SynthErrorRatios")}
)

setMethod(f = "SynthErrorRatios",
          def = function(prep, fit, treatment_time){
            pre_period <- dataprep$tag$time.plot[ ! dataprep$tag$time.plot >= treatment_time ]
            pre_error <- SynthRMSPE(prep, fit, pre_period[1], pre_period[ length(pre_period) ])
            post_period <- dataprep$tag$time.plot[ dataprep$tag$time.plot >= treatment_time ]
            post_error <- SynthRMSPE(prep, fit, post_period[1], post_period[ length(post_period) ])
            out <- list(pre = pre_error, post = post_error, ratio = post_error/pre_error)
            return(out)
          }
)


setMethod(f = "SynthErrorRatios",
          signature = "MultiSynth",
          def = function(prep, fit, treatment_time, input){
            
            pre_period <- input$tag$time.plot[input$tag$time.plot < treatment_time]
            post_period <- input$tag$time.plot[input$tag$time.plot >= treatment_time] 
            
            #Calculate the Pre- error ratios here
            PreErrors <- mapply(SynthRMSPE, prep, fit, MoreArgs = list(begin_time = pre_period[1], end_time = pre_period[length(pre_period)]), SIMPLIFY = TRUE)
            
            if(length(post_period) != 0 ){
              PostErrors <- mapply(SynthRMSPE, prep, fit, MoreArgs = list(begin_time = post_period[1], end_time = post_period[length(post_period)]), SIMPLIFY = TRUE)
              
              Ratios <- PostErrors/PreErrors
              
              out <- list(PreErrors = PreErrors, PostErrors = PostErrors, Ratios = Ratios)
              
              return(out)
              
            } else { 
              return(PreErrors)
            } #close else
          } #close function
) #close setMethod