#' Extract the Dataprep Output Esed as the Base of a MultiSynth Analysis
#' 
#' Returns the output of a call to dataprep that forms the basis of a MultiSynth analysis.  This allows the user to investigate the X0, X1, Z0, Z1, Y0plot, and Y1plot of the actual treated/full donor pool/full covariate set case. 
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return The output of the call to \code{\link{dataprep}} that forms the base of the MultiSynth object
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
           def=function(input)
           {standardGeneric("getInput")}
)

#' @rdname getInput
#' @export
setMethod(f = "getInput",
          signature = "MultiSynth",
          def = function(input){
            return(input@input)
          })

#' Extract the Data Matrices for All Cases in a MultiSynth Analysis
#' 
#' Extracts from a MultiSynth object the named list containing the X0, X1, Z0, Z1, Y0plot, and Y1plot matrices for each case. Each element of this list is a list containing the six relevent matrices for each placebo/leave-one-out case.    
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return A list of lists. Each list contains the six dataprep matrices for a single case.
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
           def=function(input)
           {standardGeneric("getPreps")}
)

#' @rdname getPreps
#' @export
setMethod(f = "getPreps",
          signature = "MultiSynth",
          def = function(input){
            return(input@preps)
          })

#' Extract the Synthetic Control Fits from a MultiSynth Object
#' 
#' Returns a list containing the \code{\link{synth}} output for each case in a MultiSynth analysis.
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
           def=function(input)
           {standardGeneric("getFits")}
)

#' @rdname getFits
#' @export
setMethod(f = "getFits",
          signature = "MultiSynth",
          def = function(input){
            return(input@fits)
          })

#' Extract Name and Number of Treated Unit in MultiSynth Analysis
#' 
#' Returns a vector containing the name and unit number of the treated unit in a MultiSynth object.
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @details If the user does not supply an arguement to \code{unit.names.variable} in the original call to \code{\link{dataprep}}, then this function returns the unit number in both elements of the output vector.  
#' 
#' @return A character vector containing the name and number of the treated unit
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
           def=function(input)
           {standardGeneric("getTreated")}
)

#' @rdname getTreated
#' @export
setMethod(f = "getTreated",
          signature = "MultiSynth",
          def = function(input){
            return(input@treated)
          })

#' Extract the Time Treatment is Administered in a MultiSynth Analysis
#' 
#' Returns the treatement is administered in a MultiSynth analysis.
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return A scalar containing the time treatment is administered
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
           def=function(input)
           {standardGeneric("getTreatmentTime")}
)

#' @rdname getTreatmentTime
#' @export
setMethod(f = "getTreatmentTime",
          signature = "MultiSynth",
          def = function(input){
            return(input@treatment_time)
          })

#' Extract Prep Matrices and Fit for One Cases in a MultiSynth Analysis
#' 
#' This function allows the user to extract the objects necessary to investigate any single case in a MultiSynth anlaysis.  It returns both the dataprep matrices and the output of the call to \code{\link{synth}}.  Together these can be used by various other functions, such as \code{\link{synth.tab}}, \code{\link{path.plot}}, \code{\link{SynthErrorRatios}}, \code{\link{SynthMeanEffect}}, and \code{\link{gaps.plot}}. 
#' 
#' @param input An object of class "MultiSynth"
#' @param case The name of one of the cases.  For a placebo analysis, this is the unit name of interest.  For leave-one-out analyses, this is "minus <<excluded unit/covaraite>>".  To see the full list of names in a MultiSynth analaysis, one can call \code{names(fitMultiSynth.out@@preps)}.
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
           def=function(input, case)
           {standardGeneric("getCase")}
)

#' @rdname getCase
#' @export
setMethod(f = "getCase",
          signature = "MultiSynth",
          def = function(input, case){
            return(list(dataprep = input@preps[case], synth.res = input@fits[case]))
          })

#' Extract the Statistics for Estimated Synthetic Control Fits in a MultiSynth Analysis
#' 
#' Returns a matrix containing the pretreatment RMSPE, posttreatment RMSPE, post- to pre-treatment RMSPE ratio, covariate loss, and ATE for each case in a MultiSynth analysis.  
#' 
#' @param input An object of class "MultiSynth"
#' 
#' @return A matrix containing the estimated statistics for each case
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
           def = function(input)
           {standardGeneric("getStats")}
)

#' @rdname getStats
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

