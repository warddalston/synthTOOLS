MultiSynthATE <- function(MultiSynthPrep_obj, MultiSynth_obj, begin_time, end_time){
  
  #Calculate the pre treatment error ratios here
  out <- mapply(SynthATE, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(begin_time = begin_time, end_time = end_time), SIMPLIFY = TRUE)

  return(out)
}