#MultiSynthAnalysis - this is a multisynth object, with added slots for all the fun stuff. 

setClass(Class="PlaceboMSAnalysis",
         contains = c("PlaceboMS"),
          representation = list(
            PreRMSPE = "numeric",
            PostRMSPE = "numeric",
            RMSPEratio = "numeric",
            CovBalances = "numeric",
            ATEs = "numeric"
          ),
          prototype = prototype(
            PreRMSPE = numeric(),
            PostRMSPE = numeric(),
            RMSPEratio = numeric(),
            CovBalances = numeric(),
            ATEs = numeric()
          )
)

setClass(Class="LOOunitsMSAnalysis",
         contains = c("LOOunitsMS"),
         representation = list(
           PreRMSPE = "numeric",
           PostRMSPE = "numeric",
           RMSPEratio = "numeric",
           CovBalances = "numeric",
           ATEs = "numeric"
         ),
         prototype = prototype(
           PreRMSPE = numeric(),
           PostRMSPE = numeric(),
           RMSPEratio = numeric(),
           CovBalances = numeric(),
           ATEs = numeric()
         )
)

setClass(Class="LOOcovariatesMSAnalysis",
         contains = c("LOOcovariatesMS"),
         representation = list(
           PreRMSPE = "numeric",
           PostRMSPE = "numeric",
           RMSPEratio = "numeric",
           CovBalances = "numeric",
           ATEs = "numeric"
         ),
         prototype = prototype(
           PreRMSPE = numeric(),
           PostRMSPE = numeric(),
           RMSPEratio = numeric(),
           CovBalances = numeric(),
           ATEs = numeric()
         )
)