#' Extract the dataprep output used as the base of a MultiSynth Analysis
#' 
#' Returns the output of a call to dataprep that forms the basis of a MultiSynth analysis.  This allows the user to investigate the X0, X1, Z0, Z1, Y0plot, and Y1plot of this main case. 
#' 
#' @usage getInput(input)
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return A list containing the output of the call to \code{\link{dataprep}} that forms the base of the MultiSynth object
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{getPreps}}
#' @seealso \code{\link{getFits}}
#' @seealso \code{\link{getTreated}}
#' @seealso \code{\link{getTreatmentTime}}
#' @seealso \code{\link{getCase}}
#' @seealso \code{\link{getStats}}
#' 
#' @rdname getInput
#' @export
setGeneric(name="getInput",
           def=function(input, ...)
           {standardGeneric("getInput")}
)

#' @export
setMethod(f = "getInput",
          signature = "MultiSynth",
          def = function(input){
            return(input@input)
          })

#' Extract the data matrices for the placebo/leave-one-out cases
#' 
#' Extracts from a MultiSynth object the named list containing the X0, X1, Z0, Z1, Y0plot, and Y1plot matrices for each of the placebo/leave-one-out cases. Each element of this list is a list containing the six relevent matrices for each placebo/leave-one-out case.    
#' 
#' @usage getPreps(input)
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return A list of lists containing the six matrices for each placebo/leave-one-out case.
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{getInput}}
#' @seealso \code{\link{getFits}}
#' @seealso \code{\link{getTreated}}
#' @seealso \code{\link{getTreatmentTime}}
#' @seealso \code{\link{getCase}}
#' @seealso \code{\link{getStats}}
#' 
#' @rdname getPreps
#' @export
setGeneric(name="getPreps",
           def=function(input, ...)
           {standardGeneric("getPreps")}
)

#' @export
setMethod(f = "getPreps",
          signature = "MultiSynth",
          def = function(input){
            return(input@preps)
          })

#' Extract the synthetic control fits from a MultiSynth object
#' 
#' Returns a list containing the output from \code{\link{synth}} for each of the placebo/leave-one-out cases.
#' 
#' @usage getFits(input)
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return A list containing the output from \code{\link{synth}} for each of the placebo/leave-one-out cases
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{getInput}}
#' @seealso \code{\link{getPreps}}
#' @seealso \code{\link{getTreated}}
#' @seealso \code{\link{getTreatmentTime}}
#' @seealso \code{\link{getCase}}
#' @seealso \code{\link{getStats}}
#' 
#' @rdname getFits
#' @export
setGeneric(name="getFits",
           def=function(input, ...)
           {standardGeneric("getFits")}
)

#' @export
setMethod(f = "getFits",
          signature = "MultiSynth",
          def = function(input){
            return(input@fits)
          })

#' Extract information about which unit is the treatment unit in a MultiSynth analysis
#' 
#' Returns a vector containing the name and unit number of the treated unit in a MultiSynth object. 
#' 
#' @usage getTreated(input)
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return A character vector containing the name and number of hte treated unit
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{getInput}}
#' @seealso \code{\link{getPreps}}
#' @seealso \code{\link{getFits}}
#' @seealso \code{\link{getTreatmentTime}}
#' @seealso \code{\link{getCase}}
#' @seealso \code{\link{getStats}}
#' 
#' @rdname getTreated
#' @export
setGeneric(name="getTreated",
           def=function(input, ...)
           {standardGeneric("getTreated")}
)

#' @export
setMethod(f = "getTreated",
          signature = "MultiSynth",
          def = function(input){
            return(input@treated)
          })

#' Extract information about when treatment occurs MultiSynth analysis
#' 
#' Returns the user-given treatement time point in a MultiSynth object
#' 
#' @usage getTreatmentTime(input)
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return A numeric vector of length one containing the time treatment is administered
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{getInput}}
#' @seealso \code{\link{getPreps}}
#' @seealso \code{\link{getFits}}
#' @seealso \code{\link{getTreated}}
#' @seealso \code{\link{getCase}}
#' @seealso \code{\link{getStats}}
#' 
#' @rdname getTreatmentTime
#' @export
setGeneric(name="getTreatmentTime",
           def=function(input, ...)
           {standardGeneric("getTreatmentTime")}
)

#' @export
setMethod(f = "getTreatmentTime",
          signature = "MultiSynth",
          def = function(input){
            return(input@treatment_time)
          })

#' Extract all information on one of the cases in a MultiSynth analysis
#' 
#' This function allows the user to extract the objects necessary to look in depth at any single case in a MultiSynth anlaysis.  It returns both the dataprep matrices and the output of the call to \code{\link{synth}}.  Together these can be used by various other functions, such as \code{\link{synth.tab}}, \code{\link{path.plot}}, and \code{\link{gaps.plot}}. 
#' 
#' @usage getCase(input, case)
#' 
#' @param input An object of class "MultiSynth"
#' @param case The name of one of the cases.  For a placebo analysis, this is the unit name of interest.  For leave-one-out analyses, this is "minus <<excluded unit/covaraite>>".
#' 
#' @return A list containing the datapre matrices and synth output for this case
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{getInput}}
#' @seealso \code{\link{getPreps}}
#' @seealso \code{\link{getFits}}
#' @seealso \code{\link{getTreated}}
#' @seealso \code{\link{getTreatmentTime}}
#' @seealso \code{\link{getStats}}
#' 
#' @rdname getCase
#' @export
setGeneric(name="getCase",
           def=function(input, case, ...)
           {standardGeneric("getCase")}
)

#' @export
setMethod(f = "getCase",
          signature = "MultiSynth",
          def = function(input, case){
            return(list(dataprep = input@preps[case], synth.res = input@fits[case]))
          })

#' Extract the statistics for estimated synthetic control fits in a MultiSynth object
#' 
#' Returns a matrix containing the pretreatment RMSPE, posttreatment RMSPE, post- to pre-treatment RMSPE ratio, covariate loss, and ATE for each of the placebo/leave-one-out cases and the main case in a MultiSynth analysis.  
#' 
#' @usage getStats(input)
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return A matrix containing the statistics for the MultiSynth cases and main case.
#' 
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{getInput}}
#' @seealso \code{\link{getPreps}}
#' @seealso \code{\link{getFits}}
#' @seealso \code{\link{getTreated}}
#' @seealso \code{\link{getTreatmentTime}}
#' @seealso \code{\link{getCase}}
#' 
#' @rdname getStats
#' @export
setGeneric(name = "getStats",
           def = function(input, ...)
           {standardGeneric("getStats")}
)

#' @export
setMethod(f = "getStats",
          signature = "MultiSynth",
          def = function(input){
            output <- cbind(input@PreRMSPE, input@PostRMSPE, input@RMSPEratio, input@CovBalances, input@ATEs)
            rownames(output) <- names(input@PreRMSPE)
            colnames(output) <- c("Pre RMSPE", "PostRMSPE", "RMSPE ratio", "Covariate Balance", "ATE")
            return(output)
          }
)

