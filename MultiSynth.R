MultiSynth <- function(MultiSynthPrep_obj, parallel = FALSE, ...){
  llply(.data = MultiSynthPrep_obj, .fun = synth, ... , .parallel = parallel)
}

setGeneric(name="fitMultiSynth",
           def=function(input, type = "placebo", treatment_time = NA, fit = TRUE, parallel=FALSE, ...)
           {standardGeneric("fitMultiSynth")}
)

setMethod(f="fitMultiSynth",
          definition=function(input, type = "placebo", treatment_time = NA, fit = TRUE, parallel=FALSE, ...){
            
            # first, create prep objects
            preps <- MultiSynthPrep(input, type)
            
            # then fit the Multi-Synth
            if(fit){
             fits <- MultiSynth(preps, parallel, ... ) 
            }
            
            
            
            return(new("MultiSynth", input = input, preps = out, fits = fits, type = type, treated = as.character(input$names.and.numbers[1,]), treatment_time = treatment_time ))
          }
)

test1 <- fitMultiSynth(IT_five_year, type = "placebo", treatment_time = 1994, fit = TRUE, parallel = TRUE)

str(test1@fits)

is(test1)
is(test1, "MultiSynth")
