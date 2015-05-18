SynthSummary <- function(dataprep, synth, treatment_time, digits = 3){
  
  RMSPE <- SynthErrorRatios(dataprep, synth, treatment_time) 
  ATE <- SynthATE(dataprep, synth, treatment_time, max(dataprep$tag$time.plot))
  tabs <- synth.tab(synth.res = synth, dataprep.res = dataprep, round.digit = digits)
  tab_print <- cbind(tabs$tab.pred, tabs$tab.v)
  colnames(tab_print) <- c("Treated", "Synthetic", "Sample Mean", "Weight")
  colnames(tabs$tab.w) <- c("Weight", "Name", "Number")
  
  cat(" ************ Covariate Values and Weights ************ \n")
  print(tab_print)
  cat( "\n************ Unit Weights ************ \n")
  print(tabs$tab.w)
  cat("\n")
  print(RMSPE)
  cat( "************ Average Treatment Effect: ", round(ATE, digits), "\n", sep = "")
  cat( "************ Covariate Loss: ", round(synth$loss.w, digits), sep = "")
}


