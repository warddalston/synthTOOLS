SynthSummary <- function(dataprep, synth, treatment_time){
  
  RMSPE <- SynthErrorRatio(dataprep, synth, treatment_time)
  pre_period <- dataprep$tag$time.plot[ ! dataprep$tag$time.plot >= treatment_time ]
  pre_error <- SynthRMSPE(dataprep, synth, pre_period[1], pre_period[ length(pre_period) ])
  
  post_period <- dataprep$tag$time.plot[ dataprep$tag$time.plot >= treatment_time ]
  post_error <- SynthRMSPE(dataprep, synth, post_period[1], post_period[ length(post_period) ])
  
  ATE <- SynthMeanEffect(dataprep, synth, treatment_time, max(dataprep$tag$time.plot))
  tabs <- synth.tab(synth.res = synth, dataprep.res = dataprep, round.digit = 2)
  
  cat(" ******** Covariate Values ********* \n")
  print(tabs$tab.pred)
  cat( "\n******** Covaraite Weights ******** \n")
  print(tabs$tab.v)
  cat( "\n******** Country Weights ******** \n")
  print(tabs$tab.w)
  cat("\n")
  print(RMSPE)
  cat( "\n******** Pre-Treatment RMSPE ******** \n")
  print(pre_error)
  cat( "\n******** Post-Treatment RMSPE ******* \n")
  print(post_error)
  cat( "\n******* ATE *********\n")
  print(ATE)
}