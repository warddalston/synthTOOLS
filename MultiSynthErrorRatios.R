
MultiSynthErrorRatios <- function(MultiSynthPrep_obj, MultiSynth_obj, treatment_time, input){
  
  pre_period <- input$tag$time.plot[input$tag$time.plot < treatment_time]
  post_period <- input$tag$time.plot[input$tag$time.plot >= treatment_time] 
  
  #Calculate the Pre- error ratios here
  PreErrors <- mapply(SynthRMSPE, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(begin_time = pre_period[1], end_time = pre_period[length(pre_period)]), SIMPLIFY = TRUE)
  
  PostErrors <- mapply(SynthRMSPE, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(begin_time = post_period[1], end_time = post_period[length(post_period)]), SIMPLIFY = TRUE)
  
  Ratios <- PostErrors/PreErrors
  
  out <- list(PreErrors = PreErrors, PostErrors = PostErrors, Ratios = Ratios)
  
  return(out)
}
