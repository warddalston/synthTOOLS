#create a basic new function, called "fitMultiSynth".  This is the workhorse of my package.  
setGeneric(name="fitMultiSynth",
           def=function(input, ...)
           {standardGeneric("fitMultiSynth")}
)

#this says what to do when fitMultiSynth is called 
setMethod(f = "fitMultiSynth",
          definition = function(input, type = "placebo", treatment_time, fit = TRUE, parallel=FALSE, ...){
            
            #so the function doesn't break if fit = FALSE
            fits <- list()
            
            #if this is a placebo analysis...
            if(type == "placebo"){
              
              preps <- PlaceboMSPrep(input)
              
              # then fit the Multi-Synth
              if(fit){
                fits <- MultiSynth(preps, parallel, ... ) 
              } 
              
              return(new("PlaceboMS", input = input, preps = out, fits = fits, treated = as.character(input$names.and.numbers[1,]), treatment_time = treatment_time ))
            } #close placebo if loop
            
            #if this is a leave one out units analysis
            if(type == "units"){
              
              preps <- LOOunitsMSPrep(input)
              
              if(fit){
                fits <- MultiSynth(preps, parallel, ... ) 
              }
              
              return(new("LOOunitsMS", input = input, preps = out, fits = fits, treated = as.character(input$names.and.numbers[1,]), treatment_time = treatment_time ))  
            } #close units if loop
            
            #if this is a leave one out covaraites 
            if(type == "covariates"){
              
              preps <- LOOcovariatesMSPrep(input)
              
              if(fit){
                fits <- MultiSynth(preps, parallel, ... ) 
              }
              
              return(new("LOOcovariatesMS", input = input, preps = out, fits = fits, treated = as.character(input$names.and.numbers[1,]), treatment_time = treatment_time ))
            } #close covariates if loop
          } #close function
) #close setMethod

#say what to do when fitMultiSynth is called on a MultiSynth object
setMethod(f = "fitMultiSynth",
          signature = "MultiSynth",
          definition = function(input, parallel=FALSE, ...){
            
              input@fits <- MultiSynth(input@preps, parallel, ... ) 
              
              return(input)
            } #close function
) #close set method          

