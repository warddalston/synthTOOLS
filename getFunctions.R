setGeneric(name="getInput",
           def=function(input, ...)
           {standardGeneric("getInput")}
)
setMethod(f = "getInput",
          signature = "MultiSynth",
          def = function(input){
            return(input@input)
          })

setGeneric(name="getPreps",
           def=function(input, ...)
           {standardGeneric("getPreps")}
)
setMethod(f = "getPreps",
          signature = "MultiSynth",
          def = function(input){
            return(input@preps)
          })

setGeneric(name="getFits",
           def=function(input, ...)
           {standardGeneric("getFits")}
)
setMethod(f = "getFits",
          signature = "MultiSynth",
          def = function(input){
            return(input@fits)
          })

setGeneric(name="getTreated",
           def=function(input, ...)
           {standardGeneric("getTreated")}
)
setMethod(f = "getTreated",
          signature = "MultiSynth",
          def = function(input){
            return(input@treated)
          })

setGeneric(name="getTreatmentTime",
           def=function(input, ...)
           {standardGeneric("getTreatmentTime")}
)
setMethod(f = "getTreatmentTime",
          signature = "MultiSynth",
          def = function(input){
            return(input@treatment_time)
          })

setGeneric(name="getCase",
           def=function(input, case, ...)
           {standardGeneric("getCase")}
)
setMethod(f = "getCase",
          signature = "MultiSynth",
          def = function(input, case){
            return(list(dataprep = input@preps[case], synth.res = input@fits[case]))
          })

setGeneric(name = "getStats",
           def = function(input, ...)
           {standardGeneric("getStats")}
)

setMethod(f = "getStats",
          signature = "MultiSynth",
          def = function(input){
            output <- cbind(input@PreRMSPE, input@PostRMSPE, input@RMSPEratio, input@CovBalances, input@ATEs)
            rownames(output) <- names(input@PreRMSPE)
            colnames(output) <- c("Pre RMSPE", "PostRMSPE", "RMSPE ratio", "Covariate Balance", "ATE")
            return(output)
          }
)

