setGeneric(name="fitMultiSynth",
           def=function(input, type = "placebo", treatment_time = NA, fit = TRUE, parallel=FALSE, ...)
           {standardGeneric("fitMultiSynth")}
)

#this says what to do when fitMultiSynth is called 
setMethod(f = "fitMultiSynth",
          definition = function(input, type = "placebo", treatment_time = NA, fit = TRUE, parallel=FALSE, ...){
            
            #if this is a placebo analysis...
            if(type == "placebo"){
              
              preps <- PlaceboMSPrep(input)
              
              # then fit the Multi-Synth
              if(fit){
                fits <- MultiSynth(preps, parallel, ... ) 
              } 
              
              return(new("PlaceboMS", input = input, preps = out, fits = fits, treated = as.character(input$names.and.numbers[1,]), treatment_time = treatment_time ))
            } #close placebo if loop
            
            if(type == "units"){
              
              preps <- LOOunitsMSPrep(input)
              
              if(fit){
                fits <- MultiSynth(preps, parallel, ... ) 
              }
              
              return(new("LOOunitsMS", input = input, preps = out, fits = fits, treated = as.character(input$names.and.numbers[1,]), treatment_time = treatment_time ))  
            } #close units if loop
            
            if(type == "covariates"){
              
              preps <- LOOcovariatesMSPrep(input)
              
              if(fit){
                fits <- MultiSynth(preps, parallel, ... ) 
              }
              
              return(new("LOOcovariatesMS", input = input, preps = out, fits = fits, treated = as.character(input$names.and.numbers[1,]), treatment_time = treatment_time ))
            } #close covariates if loop
          } #close function
) #close setMethod

test1 <- fitMultiSynth(IT_five_year, type = "placebo", treatment_time = 1994, fit = TRUE, parallel = TRUE)

str(test1@fits)

is(test1)
is(test1, "MultiSynth")
