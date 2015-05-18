
MultiSynthPathPlotPrep <- function(MultiSynthPrep_obj, MultiSynth_obj){
  
  mapply(function(prep, synth){
    
    path <- prep$Y0plot %*% synth$solution.w
    x.info <- prep$tag$time.plot
    return(list(y.info = path, x.info = x.info, skip = FALSE))
    
  }, MultiSynthPrep_obj, MultiSynth_obj, SIMPLIFY=FALSE)
}
