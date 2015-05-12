MultiSynthCovBalance <- function(MultiSynthPrep_obj, MultiSynth_obj, begin_time, end_time){
  
  #Calculate the pre treatment error ratios here
  out <- sapply(MultiSynth_obj, function(synth){ return(synth$loss.w)})
  names(out) <- sapply(MultiSynthPrep_obj, function(dataprep){ return(as.character(dataprep$names.and.numbers$unit.names[ dataprep$names.and.numbers$unit.numbers == dataprep$tag$treatment.identifier]))})
  
  #so that I can print them in a pretty way
  class(out) = "MultiSynthErrorRatio"
  return(out)
}