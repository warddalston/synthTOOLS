MultiSynthCovBalance <- function(MultiSynthPrep_obj, MultiSynth_obj){
  
  #extract the covariate balance here 
  out <- sapply(MultiSynth_obj, function(synth){ return(synth$loss.w)})
  names(out) <- sapply(MultiSynthPrep_obj, function(dataprep){ return(as.character(dataprep$names.and.numbers$unit.names[ dataprep$names.and.numbers$unit.numbers == dataprep$tag$treatment.identifier]))})
  
  #so that I can print them in a pretty way
  class(out) <- "MultiSynthCovBalance"
  comment(out) <- class(MultiSynthPrep_obj) 
  return(out)
}