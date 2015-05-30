
 # Load Hainmueller and Diamond's example data
 data(synth.data)

 # create matrices from panel data that provide inputs for fitMultiSynth()
 dataprep.out<-
  dataprep(
    foo = synth.data,
    predictors = c("X1", "X2", "X3"),
    predictors.op = "mean",
    dependent = "Y",
    unit.variable = "unit.num",
    time.variable = "year",
    special.predictors = list(
      list("Y", 1991, "mean"),
      list("Y", 1985, "mean"),
      list("Y", 1980, "mean")
    ),
    treatment.identifier = 7,
    controls.identifier = c(29, 2, 13, 17, 32, 38),
    time.predictors.prior = c(1984:1989),
    time.optimize.ssr = c(1984:1990),
    unit.names.variable = "name",
    time.plot = 1984:1996
  )
 
 # fit a placebo analysis
 fitMultiSynth.out <- fitMultiSynth(dataprep.out, treatment_time = 1991)
 
 readline("press any key to continue").
 # There is an show method defined for MultiSynth objects
 fitMultiSynth.out

 readline("press any key to continue"). 
 # There is also a summary method, which gives values for RMSPE statistics, the ATE, and covariate loss
 summary(fitMultiSynth.out)

 # plot can be used to view the distributions of these statistics one at a time
 opar <- par(mar = c(5,7, 5, 2))
 plot(fitMultiSynth.out, quantity = "Pre")
 plot(fitMultiSynth.out, quantity = "Post")
 plot(fitMultiSynth.out, quantity = "Ratios")
 plot(fitMultiSynth.out, quantity = "ATE")
 plot(fitMultiSynth.out, quantity = "Cov")
 par(opar)

 # Furthermore, MutilSynth versions of gaps.plots can be created
 gaps.plot(fitMultiSynth.out)

 # Several functions exist to extract data from a MultiSynth object:

 readline("press any key to continue").
 #the original dataprep object
 getInput(fitMultiSynth.out)

 readline("press any key to continue").
 # dataprep matrices for placebo cases
 getPreps(fitMultiSynth.out)

 readline("press any key to continue").
 # output from Synth for all fits
 getFits(fitMultiSynth.out)

 readline("press any key to continue").
 # Which case is treated?
 getTreated(fitMultiSynth.out)

 readline("press any key to continue").
 # When is treatment?
 getTreatmentTime(fitMultiSynth.out)
 
 readline("press any key to continue").
 # Pull out just the estimated fit statistics
 getStats(fitMultiSynth.out)
 
 readline("press any key to continue").
 # Extract everything necessary to analyze one case in dept:
 getCase(fitMultiSynth.out, "treated.region")

 readline("press any key to continue").
 # Leave-one-out analyses can be fitted as well
 MultiSynthLOO.out <- fitMultiSynth(dataprep.out, type = "units", treatment_time = 1991)
 
 readline("press any key to continue").
 #view this analysis
 MultiSynthLOO.out
 
 readline("press any key to continue").
 # Another option for plotting a MultiSynth analysis is path.plot
 path.plot(MultiSynthLOO.out)
