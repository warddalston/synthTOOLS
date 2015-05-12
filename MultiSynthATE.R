MultiSynthATE <- function(MultiSynthPrep_obj, MultiSynth_obj, begin_time, end_time){
  
  #Calculate the pre treatment error ratios here
  out <- mapply(SynthMeanEffect, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(begin_time = begin_time, end_time = end_time), SIMPLIFY = TRUE)
  
  #so that I can print them in a pretty way
  class(out) <- "MultiSynthATE"
  comment(out) <- class(MultiSynthPrep_obj)
  return(out)
}