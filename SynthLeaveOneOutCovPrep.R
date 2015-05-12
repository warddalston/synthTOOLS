SynthLeaveOneOutCovPrep <- function(data, predictors, dependent, unit.variable, time.variable, treatment.identifier, control.units, time.units.prior, time.units.ssr, unit.names, time.units.plot){
  
  lapply(predictors, function(covariate){
    loo_prep <- dataprep(foo = data,
                         predictors = predictors[predictors != covariate],
                         dependent = dependent,
                         unit.variable = unit.variable,
                         time.variable = time.variable,
                         treatment.identifier = treatment.identifier,
                         controls.identifier = control.units,
                         time.predictors.prior = time.units.prior,
                         time.optimize.ssr = time.units.ssr,
                         unit.names.variable = unit.names,
                         time.plot = time.units.plot)
    loo_prep$excluded <- covariate
    return(loo_prep)
  })
}
