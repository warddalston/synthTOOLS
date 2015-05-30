# Example: Hainmueller and Diamond's Toy panel dataset
data(synth.data)

# Unlike normal synth fitting, data preparation and fitting is integrated with SynthCrossVal 
synthCV.out <- 
 SynthCrossVal(
   foo = synth.data,
   predictors = c("X1", "X2", "X3"),
   predictors.op = "mean",
   special.predictors = list(
     list("Y", 1991, "mean"),
     list("Y", 1985, "mean"),
     list("Y", 1980, "mean")
   ),
   dependent = "Y",
   unit.variable = "unit.num",
   time.variable = "year",,
   treatment.identifier = 7,
   controls.identifier = c(29, 2, 13, 17, 32, 38),
   time.training = c(1984:1985),
   time.validation = c(1986:1990),
   time.optimize.final = c(1984:1990),
   unit.names.variable = "name",
   time.plot = 1984:1996
 )
 
#after fitting, Synth analysis functions can be used as normal
 print(synth.tab(synthCV.out$synth.final, synthCV.out$dataprep.final))
 
 path.plot(synthCV.out$synth.final, synthCV.out$dataprep.final, tr.intake = 1991)
 gaps.plot(synthCV.out$synth.final, synthCV.out$dataprep.final, tr.intake = 1991)

# New fuynctions from synthTools can be used as well

# Estimate the ATE
 SynthMeanEffect(synthCV.out$dataprep.final, synthCV.out$synth.final, 1991, 1996)

# Estimate RMSPE stats
 SynthErrorRatios(synthCV.out$dataprep.final, synthCV.out$synth.final, 1991)

# Put it all together with SynthSummary
 SynthSummary(synthCV.out$dataprep.final, synthCV.out$synth.final, 1991)
 