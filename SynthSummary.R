SynthSummary <- function(dataprep, synth, treatment_time, digits = 3){
  
  RMSPE <- SynthErrorRatios(dataprep, synth, treatment_time) 
  ATE <- SynthMeanEffect(dataprep, synth, treatment_time, max(dataprep$tag$time.plot))
  tabs <- synth.tab(synth.res = synth, dataprep.res = dataprep, round.digit = 2)
  
  cat(" ************ Covariate Values ************ \n")
  print(tabs$tab.pred)
  cat( "\n************ Covariate Weights ************ \n")
  print(tabs$tab.v)
  cat( "\n************ Unit Weights ************ \n")
  print(tabs$tab.w)
  cat("\n")
  print(RMSPE)
  cat( " ************ Average Treatment Effect: ", round(ATE, digits), sep = "")
}