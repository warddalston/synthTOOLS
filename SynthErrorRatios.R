SynthErrorRatios <- function(dataprep, synth, treatment_time){
  pre_period <- dataprep$tag$time.plot[ ! dataprep$tag$time.plot >= treatment_time ]
  pre_error <- SynthRMSPE(dataprep, synth, pre_period[1], pre_period[ length(pre_period) ])
  post_period <- dataprep$tag$time.plot[ dataprep$tag$time.plot >= treatment_time ]
  post_error <- SynthRMSPE(dataprep, synth, post_period[1], post_period[ length(post_period) ])
  out <- list(pre = pre_error, post = post_error, ratio = post_error/pre_error)
  class(out) <- "SynthErrorRatios"
  return(out)
}
