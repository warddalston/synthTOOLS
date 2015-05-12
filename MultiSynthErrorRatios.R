
MultiSynthErrorRatios <- function(MultiSynthPrep_obj, MultiSynth_obj, treatment_time){
  
  #Calculate the error ratios here
  out <- mapply(SynthErrorRatio, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(treatment_time = treatment_time), SIMPLIFY = TRUE)
  #so that I can print them in a pretty way
  class(out) = "MultiSynthErrorRatio"
  return(out)
}
