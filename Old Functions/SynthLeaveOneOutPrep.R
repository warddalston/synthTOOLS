SynthLeaveOneOutPrep <- function(data, predictors, dependent, unit.variable, time.variable, treatment.identifier, control.units, time.units.prior, time.units.ssr, unit.names, time.units.plot){
  
  lapply(control.units, function(case){
    loo_prep <- dataprep(foo = data,
                         predictors = predictors,
                         dependent = dependent,
                         unit.variable = unit.variable,
                         time.variable = time.variable,
                         treatment.identifier = treatment.identifier,
                         controls.identifier = control.units[ ! control.units == case ],
                         time.predictors.prior = time.units.prior,
                         time.optimize.ssr = time.units.ssr,
                         unit.names.variable = unit.names,
                         time.plot = time.units.plot)
    loo_prep$excluded <- unique(data[,unit.names][data[,unit.variable] == case])
    return(loo_prep)
  })
}
