MultiSynthPlotLines <- function(MultiSynthPlotPrep_obj, col = "gray48", lwd = 3/4, cex = 1, t = "l"){
  lapply(MultiSynthPlotPrep_obj,function(controlCase){
    if(!controlCase$skip){
      lines(controlCase$x.info, controlCase$y.info, t = t, col = col, lwd = lwd, cex = cex)
    }
  }) # close lapply
} # close function 
