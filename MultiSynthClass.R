setClass(Class="MultiSynth", 
         representation = list(
           input = "list",
           preps = "list",
           fits = "list",
           treated = "character",
           treatment_time = "numeric",
           PreRMSPE = "numeric",
           PostRMSPE = "numeric",
           RMSPEratio = "numeric",
           CovBalances = "numeric",
           ATEs = "numeric"
         ),
         prototype = prototype(
           input = list(),
           preps = list(),
           fits = list(),
           treated = NA_character_,
           treatment_time = NA_real_,
           PreRMSPE = numeric(),
           PostRMSPE = numeric(),
           RMSPEratio = numeric(),
           CovBalances = numeric(),
           ATEs = numeric()
         )
)

setClass("PlaceboMS", contains = "MultiSynth",
         representation = list(
           p_value = "numeric"),
         prototype = prototype(
           p_value = numeric()
           )
         )
setClass("LOOunitsMS", contains = "MultiSynth")
setClass("LOOcovariatesMS", contains = "MultiSynth")


setMethod("initialize", "MultiSynth", 
          function(.Object, input = list(), preps = list(), fits = list(), treated = character(), treatment_time = numeric(), PreRMSPE = numeric(), PostRMSPE = numeric(), RMSPEratio = numeric(), CovBalances = numeric(), ATEs = numeric()
                   ){ 
            .Object@input <- input
            .Object@preps <- preps
            .Object@fits <- fits
            .Object@treated <- treated
            .Object@treatment_time <- treatment_time
            .Object@PreRMSPE <- PreRMSPE
            .Object@PostRMSPE <- PostRMSPE
            .Object@RMSPEratio <- RMSPEratio
            .Object@CovBalances <- CovBalances
            .Object@ATEs <- ATEs
            .Object
          }
)

setMethod("initialize", "PlaceboMS", 
          function(.Object, input = list(), preps = list(), fits = list(), treated = character(), treatment_time = numeric(), PreRMSPE = numeric(), PostRMSPE = numeric(), RMSPEratio = numeric(), CovBalances = numeric(), ATEs = numeric(), p_value = numeric()
          ){ 
            .Object@input <- input
            .Object@preps <- preps
            .Object@fits <- fits
            .Object@treated <- treated
            .Object@treatment_time <- treatment_time
            .Object@PreRMSPE <- PreRMSPE
            .Object@PostRMSPE <- PostRMSPE
            .Object@RMSPEratio <- RMSPEratio
            .Object@CovBalances <- CovBalances
            .Object@ATEs <- ATEs
            .Object@p_value <- p_value
            .Object
          }
)