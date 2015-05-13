setClass(Class="MultiSynth", 
         representation = list(
           input = "list",
           preps = "list",
           fits = "list",
           treated = "character",
           treatment_time = "numeric"
         ),
         prototype = prototype(
           input = list(),
           preps = list(),
           fits = list(),
           treated = NA_character_,
           treatment_time = NA_real_
         )
)

setClass("PlaceboMS", contains = "MultiSynth")
setClass("LOOunitsMS", contains = "MultiSynth")
setClass("LOOcovariatesMS", contains = "MultiSynth")


setMethod("initialize", "MultiSynth", 
          function(.Object, input = list(), preps = list(), fits = list(), treated = character(), treatment_time = numeric()){ 
            
            #             #below are three basic checks to make sure that the user is giving us the proper information for a BMA analysis. 
            #             if(any(is.na(X))){
            #               stop("The function does not accept covariate matrices with missing values")
            #             }
            #             if(any(is.na(y))){
            #               stop("The function does not accept outcome vectors with missing values")
            #             }
            #             if(!length(y)==0 & length(y)!=nrow(X)){ #the first part of this logical makes the default BMA object not trigger this if loop.  
            #               stop("The length of y and the number of rows of X must be equal")
            #             }
            .Object@input <- input
            .Object@preps <- preps
            .Object@fits <- fits
            .Object@treated <- treated
            .Object@treatment_time <- treatment_time
            .Object
          }
)