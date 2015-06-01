
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
 
 # There is an show method defined for MultiSynth objects
 readline("press any key to continue")
 fitMultiSynth.out

 
 # There is also a summary method, which gives values for RMSPE statistics, the ATE, and covariate loss
 readline("press any key to continue")
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


 #the original dataprep object
 readline("press any key to continue")
 getInput(fitMultiSynth.out)

 # dataprep matrices for placebo cases
 readline("press any key to continue")
 getPreps(fitMultiSynth.out)

 # output from Synth for all fits
 readline("press any key to continue")
 getFits(fitMultiSynth.out)

 # Which case is treated?
 readline("press any key to continue")
 getTreated(fitMultiSynth.out)

 # When is treatment?
 readline("press any key to continue")
 getTreatmentTime(fitMultiSynth.out)
 
 # Pull out just the estimated fit statistics
 readline("press any key to continue")
 getStats(fitMultiSynth.out)
 
 # Extract everything necessary to analyze one case in dept:
 readline("press any key to continue")
 getCase(fitMultiSynth.out, "treated.region")

 # Leave-one-out analyses can be fitted as well
 readline("press any key to continue")
 MultiSynthLOO.out <- fitMultiSynth(dataprep.out, type = "units", treatment_time = 1991)
 
 #view this analysis
 readline("press any key to continue")
 MultiSynthLOO.out
 
 # Another option for plotting a MultiSynth analysis is path.plot
 path.plot(MultiSynthLOO.out)
