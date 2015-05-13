MultiSynthCovBalance <- function(MultiSynthPrep_obj, MultiSynth_obj){
  
  #extract the covariate balance here 
  out <- sapply(MultiSynth_obj, function(synth){ return(synth$loss.w)})
  
  #so that I can print them in a pretty way
  class(out) <- "MultiSynthCovBalance"
  comment(out) <- class(MultiSynthPrep_obj) 
  return(out)
}