MultiSynth <- function(MultiSynthPrep_obj, parallel = FALSE, ...){
  llply(.data = MultiSynthPrep_obj, .fun = synth, ... , .parallel = parallel)
}