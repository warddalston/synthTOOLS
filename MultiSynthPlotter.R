MultiSynthPlotter <- function(MultiSynthPrep_obj, MultiSynth_obj, gap = TRUE, main_case, ...){
  if(gap){
    temp <- MultiSynthGapPlotPrep(MultiSynthPrep_obj, MultiSynth_obj, main_case )
  } else {
    temp <- MultiSynthPathPlotPrep(MultiSynthPrep_obj, MultiSynth_obj)
  }
  MultiSynthPlotLines(temp, ...)
}