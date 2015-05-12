SynthErrorRatio <- function(dataprep, synth, treatment_time){
  pre_period <- dataprep$tag$time.plot[ ! dataprep$tag$time.plot >= treatment_time ]
  pre_error <- SynthRMSPE(dataprep, synth, pre_period[1], pre_period[ length(pre_period) ])
  # print(pre_error)
  post_period <- dataprep$tag$time.plot[ dataprep$tag$time.plot >= treatment_time ]
  post_error <- SynthRMSPE(dataprep, synth, post_period[1], post_period[ length(post_period) ])
  # print(post_error)
  out <- post_error/pre_error
  if(!is.null(dataprep$excluded)){
    names(out) <- as.character(dataprep$excluded)
  } else {
    names(out) <-  as.character(dataprep$names.and.numbers$unit.names[ dataprep$names.and.numbers$unit.numbers == dataprep$tag$treatment.identifier])
  }
  class(out) <- "SynthErrorRatio"
  return(out)
}
