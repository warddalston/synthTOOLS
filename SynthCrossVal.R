SynthCrossVal <- function(foo = NULL,
                          predictors = NULL,
                          predictors.op = "mean",
                          special.predictors = NULL,
                          dependent = NULL,
                          unit.variable = NULL,
                          time.variable = NULL,
                          treatment.identifier = NULL,
                          controls.identifier = NULL,
                          time.training = NULL,
                          time.validation = NULL,
                          time.optimize.final = NULL,
                          time.plot = time.optimize.final,
                          unit.names.variable = NA,
                          ...
                          ){
  
  dataprep.training <- dataprep(foo = foo,
                                predictors = predictors,
                                predictors.op = predictors.op,
                                special.predictors = special.predictors,
                                dependent = dependent,
                                unit.variable = unit.variable,
                                time.variable = time.variable,
                                treatment.identifier = treatment.identifier,
                                controls.identifier = controls.identifier,
                                time.predictors.prior = time.training,
                                time.optimize.ssr = time.validation,
                                time.plot = time.plot,
                                unit.names.variable = unit.names.variable
                                )
  
  synth.training <- synth(dataprep.training, ...)
  
  dataprep.final <- dataprep(foo = foo,
                             predictors = predictors,
                             predictors.op = predictors.op,
                             special.predictors = special.predictors,
                             dependent = dependent,
                             unit.variable = unit.variable,
                             time.variable = time.variable,
                             treatment.identifier = treatment.identifier,
                             controls.identifier = controls.identifier,
                             time.predictors.prior = time.validation,
                             time.optimize.ssr = time.optimize.final,
                             time.plot = time.plot,
                             unit.names.variable = unit.names.variable)
  
  synth.final <- synth(dataprep.final, custom.v = as.numeric(synth.training$solution.v), ...)
  
  return(list(dataprep.training = dataprep.training, synth.training = synth.training, dataprep.final = dataprep.final, synth.final = synth.final))
  
}