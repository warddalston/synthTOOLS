MultiSynthPlotLines <- function(MultiSynthPlotPrep_obj, col = "gray48", lwd = 3/4, cex = 1, t = "l"){
  lapply(MultiSynthPlotPrep_obj,function(controlCase){
    lines(controlCase$x.info, controlCase$y.info, t = t, col = col, lwd = lwd, cex = cex)
  }) # close lapply
} # close function 


MultiSynthGapPlotPrep <- function(MultiSynthPrep_obj, MultiSynth_obj){ #the inputs are lists.  
  
  mapply(function(prep, synth){
    
    gap <- prep$Y1plot - (prep$Y0plot %*% synth$solution.w)
    x.info <- as.numeric(rownames(prep$Y0plot))
    
    return(list(y.info = gap, x.info = x.info))
    
  }, MultiSynthPrep_obj, MultiSynth_obj, SIMPLIFY=FALSE)
} # close function


MultiSynthPathPlotPrep <- function(MultiSynthPrep_obj, MultiSynth_obj){
  
  if(length(MultiSynthPrep_obj) != length(MultiSynth_obj)){
    stop("There should be the same number of Preps and fits! Something's gone wrong")
  }
  
  mapply(function(prep, synth){
    
    path <- prep$Y0plot %*% synth$solution.w
    x.info <- as.numeric(rownames(prep$Y0plot))
    return(list(y.info = path, x.info = x.info))
    
  }, MultiSynthPrep_obj, MultiSynth_obj, SIMPLIFY=FALSE)
}