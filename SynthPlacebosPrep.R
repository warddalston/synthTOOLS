SynthPlacebosPrep <- function(data, predictors, dependent, unit.variable, time.variable, control.units, time.units.prior, time.units.ssr, unit.names, time.units.plot){
  
  lapply(control.units, function(case){
    dataprep(foo = data,
             predictors = predictors,
             dependent = dependent,
             unit.variable = unit.variable,
             time.variable = time.variable,
             treatment.identifier = case,
             controls.identifier = control.units[ !control.units == case],
             time.predictors.prior = time.units.prior,
             time.optimize.ssr = time.units.ssr,
             unit.names.variable = unit.names,
             time.plot = time.units.plot)
  })
}
