SynthRMSPE <- function(dataprep, synth, begin_time, end_time){
  time <- row.names(dataprep$Y1plot) %in% as.character(begin_time : end_time)
  gaps <- dataprep$Y1plot - (dataprep$Y0plot %*% synth$solution.w)
  gaps.sq <- gaps^2
  mean.gaps <- mean(gaps.sq[time])
  return(sqrt(mean.gaps))
}