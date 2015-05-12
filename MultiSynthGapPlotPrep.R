MultiSynthGapPlotPrep <- function(MultiSynthPrep_obj, MultiSynth_obj, main_case){ #the inputs are lists.  
  
  mapply(function(prep, synth, main_case){
    
    if(prep$tag$treatment.identifier != main_case){
      skip <- FALSE
    } else { skip <- TRUE }
    
    gap <- prep$Y1plot - (prep$Y0plot %*% synth$solution.w)
    x.info <- prep$tag$time.plot
    
    return(list(y.info = gap, x.info = x.info, skip = skip))
    
  }, MultiSynthPrep_obj, MultiSynth_obj, MoreArgs = list(main_case = main_case), SIMPLIFY=FALSE)
} # close function
