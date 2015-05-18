#create a basic new function, called "fitMultiSynth".  This is the workhorse of my package.  
setGeneric(name="fitMultiSynth",
           def=function(input, ...)
           {standardGeneric("fitMultiSynth")}
)

#this says what to do when fitMultiSynth is called 
setMethod(f = "fitMultiSynth",
          definition = function(input, type = "placebo", treatment_time = NA, fit = TRUE, parallel=FALSE, ...){
            
            if(length(treatment_time) != 1 | !is.numeric(treatment_time) | is.na(treatment_time)){
              stop("Please enter a single, numeric, treatment time.")
            } 
            
            if(all(input$tag$time.plot < treatment_time) ){
              stop("Time plot values have no post-treatment periods. \nMultiSynth requires these for plotting and estimating statistics. \nPlease rerun dataprep and enter these!")
            }
            
            #so the function doesn't break if fit = FALSE
            fits <- list()
            
            #if this is a placebo analysis...
            if(type == "placebo"){
              
              preps <- PlaceboMSPrep(input)
              
              # then fit the Multi-Synth
              if(fit){
                fits <- MultiSynth(preps, parallel, ... ) 
                names(fits) <- names(preps)
                RMSPES <- MultiSynthErrorRatios(preps, fits, treatment_time, input)
                Cov <- sapply(fits, function(synth){ return(synth$loss.w)})
                post_period <- input$tag$time.plot[input$tag$time.plot > treatment_time]
                ATEs <- MultiSynthATE(preps, fits, post_period[1], post_period[length(post_period)])
                p_value = sum(RMSPES[[3]] >= RMSPES[[3]][1])/length(RMSPES[[3]])
              } 
              
              return(new("PlaceboMS", input = input, preps = preps, fits = fits, treated = c(as.character(input$names.and.numbers[1,1]), as.character(input$names.and.numbers[1,2]) ), treatment_time = treatment_time, PreRMSPE = RMSPES[[1]], PostRMSPE = RMSPES[[2]], RMSPEratio = RMSPES[[3]], CovBalances = Cov, ATEs = ATEs, p_value = p_value))
            } #close placebo if loop
            
            #if this is a leave one out units analysis
            if(type == "units"){
              
              preps <- LOOunitsMSPrep(input)
              
              if(fit){
                fits <- MultiSynth(preps, parallel, ... ) 
                names(fits) <- names(preps)
                RMSPES <- MultiSynthErrorRatios(preps, fits, treatment_time, input)
                Cov <- sapply(fits, function(synth){ return(synth$loss.w)})
                post_period <- input$tag$time.plot[input$tag$time.plot > treatment_time]
                ATEs <- MultiSynthATE(preps, fits, post_period[1], post_period[length(post_period)])
              }
              
              return(new("LOOunitsMS", input = input, preps = preps, fits = fits, treated = c(as.character(input$names.and.numbers[1,1]), as.character(input$names.and.numbers[1,2]) ), treatment_time = treatment_time, PreRMSPE = RMSPES[[1]], PostRMSPE = RMSPES[[2]], RMSPEratio = RMSPES[[3]], CovBalances = Cov, ATEs = ATEs))  
            } #close units if loop
            
            #if this is a leave one out covaraites 
            if(type == "covariates"){
              
              preps <- LOOcovariatesMSPrep(input)
              
              if(fit){
                fits <- MultiSynth(preps, parallel, ... ) 
                names(fits) <- names(preps)
                RMSPES <- MultiSynthErrorRatios(preps, fits, treatment_time, input)
                Cov <- sapply(fits, function(synth){ return(synth$loss.w)})
                post_period <- input$tag$time.plot[input$tag$time.plot > treatment_time]
                ATEs <- MultiSynthATE(preps, fits, post_period[1], post_period[length(post_period)])
              }
              
              return(new("LOOcovariatesMS", input = input, preps = preps, fits = fits, treated = c(as.character(input$names.and.numbers[1,1]), as.character(input$names.and.numbers[1,2]) ), treatment_time = treatment_time, PreRMSPE = RMSPES[[1]], PostRMSPE = RMSPES[[2]], RMSPEratio = RMSPES[[3]], CovBalances = Cov, ATEs = ATEs))
            } #close covariates if loop
          } #close function
) #close setMethod

#say what to do when fitMultiSynth is called on a MultiSynth object
setMethod(f = "fitMultiSynth",
          signature = "MultiSynth",
          definition = function(input, parallel=FALSE, ...){
            
              input@fits <- MultiSynth(input@preps, parallel, ... ) 
              names(fits) <- names(preps)
              RMSPES <- MultiSynthErrorRatios(preps, fits, treatment_time, input)
              input@CovBalances <- sapply(fits, function(synth){ return(synth$loss.w)})
              post_period <- input$tag$time.plot[input$tag$time.plot > treatment_time]
              input@ATEs <- MultiSynthATE(preps, fits, post_period[1], post_period[length(post_period)])
              
              return(input)
            } #close function
) #close set method          
# 
# try1 <- fitMultiSynth(IT_five_year, type = "units", treatment_time = 1994, parallel = TRUE)
# try2 <- fitMultiSynth(IT_five_year, type = "covariates", treatment_time = 1994, parallel = TRUE)
# try3 <- fitMultiSynth(IT_five_year, treatment_time = 1994, parallel = TRUE)
# 
