SynthMeanEffect <- function(dataprep, synth, begin_time, end_time){
  time <- row.names(dataprep$Y1plot) %in% as.character(begin_time : end_time)
  gaps <- dataprep$Y1plot - (dataprep$Y0plot %*% synth$solution.w)
  out <- mean(gaps[time])
  return(out)
  #  return(mean(dataprep$Y1plot[time] - (dataprep$Y0plot[time] %*% synth$solution.w[time])))
}