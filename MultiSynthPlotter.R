MultiSynthPlotter <- function(MultiSynthPrep_obj, MultiSynth_obj, gap = TRUE, main_case, ...){
  if(gap){
    temp <- MultiSynthGapPlotPrep(MultiSynthPrep_obj, MultiSynth_obj, main_case )
  } else {
    temp <- MultiSynthPathPlotPrep(MultiSynthPrep_obj, MultiSynth_obj)
  }
  MultiSynthPlotLines(temp, ...)
}



MultiSynthPlotLines <- function(MultiSynthPlotPrep_obj, col = "gray48", lwd = 3/4, cex = 1, t = "l"){
  lapply(MultiSynthPlotPrep_obj,function(controlCase){
    if(!controlCase$skip){
      lines(controlCase$x.info, controlCase$y.info, t = t, col = col, lwd = lwd, cex = cex)
    }
  }) # close lapply
} # close function 


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


MultiSynthPathPlotPrep <- function(MultiSynthPrep_obj, MultiSynth_obj){
  
  if(length(MultiSynthPrep_obj) != length(MultiSynth_obj)){
    stop("There should be the same number of Preps and fits! Something's gone wrong")
  }
  
  mapply(function(prep, synth){
    
    path <- prep$Y0plot %*% synth$solution.w
    x.info <- as.numeric(rownames(prep$Y0plot))
    return(list(y.info = path, x.info = x.info, skip = FALSE))
    
  }, MultiSynthPrep_obj, MultiSynth_obj, SIMPLIFY=FALSE)
}

#gap plot for MultiSynth
setMethod(f = "path.plot",
          signature = "MultiSynth",
          def = function(synth.res = input, dataprep.res = NA, tr.intake = NA, Ylab = c("Y Axis"), Xlab = c("Time"), Ylim = NA, Legend = c("Treated", "Synthetic"), Legend.position = c("topright"), Main = NA, Z.plot = FALSE){
          
            
           #plot the basic thing
           path.plot(synth.res@fits[[1]], synth.res@input, synth.res@treatment_time)
          
           #prep the additional lines
           plot_prep <- MultiSynthPathPlotPrep(synth.res@preps[2:length(synth.res@preps)], synth.res@fits[2:length(synth.res@fits)] )
           
           print(plot_prep)
           
           #plot new lines
           MultiSynthPlotLines(plot_prep)
           
            
            return(invisible())
          } #end function
          ) #end setMethod

