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

#gap plot for MultiSynth
setMethod(f = "path.plot",
          signature = "MultiSynth",
          def = function(synth.res = input, dataprep.res = NA, tr.intake = NA, Ylab = c("Y Axis"), Xlab = c("Time"), Ylim = NA, Legend = c("Treated", "Synthetic"), Legend.position = c("topright"), Main = NA, Z.plot = FALSE){
            
           #plot the basic thing
           path.plot(synth.res@fits[[1]], synth.res@input, synth.res@treatment_time, Ylab = Ylab, Xlab= Xlab, Ylim = Ylim, Legend = Legend, Legend.position = Legend.position, Main = Main, Z.plot = Z.plot)
          
           #prep the additional lines
           plot_prep <- MultiSynthPathPlotPrep(synth.res@preps[2:length(synth.res@preps)], synth.res@fits[2:length(synth.res@fits)] )
                      
           #plot new lines
           MultiSynthPlotLines(plot_prep)  
            
            return(invisible())
          } #end function
          ) #end setMethod

setMethod(f = "gaps.plot",
          signature = "MultiSynth",
          def = function(synth.res = input, dataprep.res = NA, Ylab = c("Title"),Xlab = c("Time"), Main = c("Gaps: Treated - Synthetic"), tr.intake = NA, Ylim = NA, Z.plot = FALSE){
            
            #prep additional lines 
            plot_prep <- MultiSynthGapPlotPrep(synth.res@preps[2:length(synth.res@preps)], synth.res@fits[2:length(synth.res@fits)] )
            
            Max_gap <- max(abs(synth.res@input$Y1plot - (synth.res@input$Y0plot %*% synth.res@fits[[1]]$solution.w))) 
            
            y_lim_gaps <- if(any(sapply(plot_prep, function(list){ max(abs(list$y.info)) > Max_gap})) ){max(sapply(plot_prep, function(list){ max(abs(list$y.info) )} ) ) }  else { Max_gap} 

            y_lim_final <- if(is.na(Ylim[1])){c(-1*(y_lim_gaps + .3*y_lim_gaps), y_lim_gaps + .3*y_lim_gaps)} else {Ylim}
            
            #plot the basic thing
            gaps.plot(synth.res@fits[[1]], synth.res@input, synth.res@treatment_time, Ylab = Ylab, Xlab= Xlab, Ylim = y_lim_final, Main = Main, Z.plot = Z.plot)
            
            #plot new lines
            MultiSynthPlotLines(plot_prep)  
            
            return(invisible())
          })








