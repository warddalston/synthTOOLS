#' Perform a MultiSynth Analysis 
#' 
#' Implements placebo analyses and leave-one-out analyses for synthetic control studies, as described by Abadie, Diamond, and Hainmueller (2010, 2015).  \code{fitMultSynth} performs these analyses by building off of Hainmueller and Diamond's \code{synth} package.  
#' 
#'  Three types of analyses are implemented through \code{fitMultiSynth}: in-space placebo analyses, leave-one-out units analyses, and leave-one-out covariates analyses.  In a placebo analysis, synthetic controls are iteratively fit, with each iteration reassigning the country to be considered as treated.  In a leave-one-out units analysis, synthetic controls are iteratively fit, each time ommitting a single unit from the donor pool.  In a leave-one-out covariates analysis, synthetic controls are iteratively fit, with each iteration omitting a single covariate from set of covariates.       
#'  
#'  \code{fitMultiSynth} performs these analyses by creating all of the necessary matricies for these placebo/leave-one-out cases using only the output of the \code{dataprep} function and the user supplied time of treatement.  It first iteratively creates the data objects necessary for the analyses, and then fits all of the necessary synthetic controls.  Finally, it calculates a number of statistics which can be used for making inferences on the quality of a synthetic control analysis.  
#'  
#'    \code{fitMultiSynth} requires the user to supply the output of a call to the \code{dataprep} function as its main input and the time treatment is administered as the main inputs.  It also requires the user to choose what type of analysis to perform, either "placebo", "units" or "covaraites".  Finally, the user has the option to run the anlaysis in parallel, to speed computation of potentially many synthetic control fits.  
#'    
#'      The output of \code{fitMultiSynth} is an object of class \code{\link{MultiSynth}}.  This object contains the input data, and the matricies and output for the placebo/leave-one-out anlaysis, information on the treated case and treatment time, and vectors including several statstics about the placebo/leave-one-out synthetic fits. The functions \code{summary}, \code{plot}, \code{path.plot}, and \code{gaps.plot} all have methods for \code{MultiSynth} which allow for summarizing the results of a placebo/leave-one-out analysis.  
#'      
#'    
#' @param input The output of a call to \code{dataprep}.  Note, that the call to dataprep must include values for the arguement "time.plot" that include the post-treatment period as well as the pre-treatment period.  Also, including character unit names is highly advised. 
#' 
#' @param type A string describing what type of MultiSynth analysis to perform.  Defaults to "placebo".  Other options include "units" for a leave-one-out units analysis, and "covariates" for a leave-one-out covariates
#' 
#' @param treatment_time A numeric of length 1 giving the time of treatment uptake.
#' 
#' @param parallel Logical flag.  If TRUE, than fitting is done in parallel to increase speed.  
#' 
#' @param ... Additional arguements to be passed to \code{\link{synth}}
#' 
#'@details \code{fitMultiSynth} performs the iterative significance and robustness tests as described by Abadie, Diamond, Hainmueller (2010) by first creating the necessary matrices of predictors and outcomes for each placebo/leave-one-out analysis, and then fitting synthetic controls for each placebo/leave-one-out case. The prep matrices are created by a call to one of three functions: \code{\link{PlaceboMSPrep}}, \code{\link{LOOunitsMSPrep}}, or \code{\link{LOOcovariatesMSPrep}}.  
#'
#' The function also returns several statistics useful in summarizing a placebo/leave-one-out analysis.  These are: the Root Mean Square Predictive Error (RMSPE) in the pre-treatment phase, the RMSPE in the post-treatment phase, the ratio of post- to pre-treatment RMSPE, covariate loss, and the average treatment effect in the post-treatment phase.  
#' 
#' @return An object of class \code{\link{MultiSynth}} containing 
#' \itemize{
#' \item \code{input} The dataprep object for synthetic control anlaysis being studied.
#' \item \code{preps} A list with the X0, X1, Z0, Z1, Y0plot, and Y1plot for each of the placebo/leave-one-out case and the treated/full donor pool/full covariate set case.
#' \item \code{fits}  A list with the output of \code{synth} for each of the placebo/leave-one-out cases and the treated/full donor pool/full covariate set case.
#' ' \item \code{treated} A character vector of length two giving the name and number of the treated unit (if names are not supplied, the number is repeated)
#' \item \code{treatment_time} A scalar giving the time treatment is recieved
#' \item \code{PreRMSPE} A numeric vector giving the root mean square predictive error (RMSPE) in the pre-treatment period for each case. 
#' \item \code{PostRMSPE} A numeric vector giving the RMSPE in the post-treatment period for each case. 
#' \item \code{RMSPEratio} A numeric vector giving the ratio of post-treatment RMSPE to pre-treatment RMSPE for each case.
#' \item \code{CovBalances} A numeric vector giving the covariate loss for each case.
#' \item \code{ATEs} A numeric vector giving the average treatment effect (ATE) for each case.
#' \item \code{p_value} A scalar with the exact p-value of getting an RMSPE ratio as high as the treated unit, if choosing a unit to analyze at random. Only in \code{PlaceboMS} objects. 
#'  }
#'
#' @author Dalston G. Ward: \email{ward.dalston@@wustl.edu}
#' @references \itemize{
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2010). Synthetic Control Methods for Comparative Case Studies: Estimating the Effect of California's Tobacco Control Program. \emph{Journal of the American Statistical Association} 105 (490) 493-505.
#' \item Abadie, A., Diamond, A., Hainmueller, J. (2015). Comparative Politics and the Synthetic Control Method.  \emph{American Journal of Political Science} 59 (2) 495-510
#' }
#' 
#' @seealso \code{\link{MultiSynth}}
#' @seealso \code{\link{plot,MultiSynth-method}}
#' @seealso \code{\link{summary,MultiSynth-method}}
#' @seealso \code{\link{path.plot,MultiSynth-method}}
#' @seealso \code{\link{gaps.plot,MultiSynth-method}}
#' 
#' @examples
#' 
#' \dontrun{
#' ##Example: Hainmueller and Diamond's Toy panel dataset
#'
#'  #load data
#' data(synth.data)
#'
#' ## create matrices from panel data that provide inputs for fitMultiSynth()
#' dataprep.out<-
#'  dataprep(
#'    foo = synth.data,
#'    predictors = c("X1", "X2", "X3"),
#'    predictors.op = "mean",
#'    dependent = "Y",
#'    unit.variable = "unit.num",
#'    time.variable = "year",
#'    special.predictors = list(
#'      list("Y", 1991, "mean"),
#'      list("Y", 1985, "mean"),
#'      list("Y", 1980, "mean")
#'    ),
#'    treatment.identifier = 7,
#'    controls.identifier = c(29, 2, 13, 17, 32, 38),
#'    time.predictors.prior = c(1984:1989),
#'    time.optimize.ssr = c(1984:1990),
#'    unit.names.variable = "name",
#'    time.plot = 1984:1996
#'  )
#'  
#'  ## The above dataprep call represents the "main case".
#'  ## Using this as input, call fitMultiSynth().
#'  ## This will fit a synthetic control to each placebo/leave-one-out case
#'  
#'  fitMultiSynth.out <- fitMultiSynth(dataprep.out, treatment_time = 1991)
#'  
#'  ## We can analyze the placebo analyses in several ways. 
#'  
#'  summary(fitMultiSynth.out) 
#'  plot(fitMultiSynth.out)
#'  gaps.plot(fitMultiSynth.out)
#' }
#' 
#' @import Synth
#' @import methods
#' @rdname fitMultiSynth
#' @export
fitMultiSynth <- function(input, type = "placebo", treatment_time = NA, parallel=FALSE, ...){
            
            if(length(treatment_time) != 1 | !is.numeric(treatment_time) | is.na(treatment_time)){
              stop("Please enter a single, numeric, treatment time.")
            } 
            
            if(all(input$tag$time.plot < treatment_time) ){
              stop("Time plot values have no post-treatment periods. \nMultiSynth requires these for plotting and estimating statistics. \nPlease rerun dataprep and enter these!")
            }
            
            #if this is a placebo analysis...
            if(type == "placebo"){
              
              preps <- PlaceboMSPrep(input)
              
              # then fit the Multi-Synth
                fits <- plyr::llply(.data = preps, .fun = synth, ... , .parallel = parallel)
                names(fits) <- names(preps)
                RMSPES <- MultiSynthErrorRatios(preps, fits, treatment_time, input)
                Cov <- sapply(fits, function(synth){ return(synth$loss.w)})
                post_period <- input$tag$time.plot[input$tag$time.plot > treatment_time]
                ATEs <- MultiSynthATE(preps, fits, post_period[1], post_period[length(post_period)])
                p_value = sum(RMSPES[[3]] >= RMSPES[[3]][1])/length(RMSPES[[3]])
              
              
              return(new("PlaceboMS", input = input, preps = preps, fits = fits, treated = c(as.character(input$names.and.numbers[1,1]), as.character(input$names.and.numbers[1,2]) ), treatment_time = treatment_time, PreRMSPE = RMSPES[[1]], PostRMSPE = RMSPES[[2]], RMSPEratio = RMSPES[[3]], CovBalances = Cov, ATEs = ATEs, p_value = p_value))
            } #close placebo if loop
            
            #if this is a leave one out units analysis
            if(type == "units"){
              
              preps <- LOOunitsMSPrep(input)
              
              fits <- plyr::llply(.data = preps, .fun = synth, ... , .parallel = parallel)
              names(fits) <- names(preps)
                RMSPES <- MultiSynthErrorRatios(preps, fits, treatment_time, input)
                Cov <- sapply(fits, function(synth){ return(synth$loss.w)})
                post_period <- input$tag$time.plot[input$tag$time.plot > treatment_time]
                ATEs <- MultiSynthATE(preps, fits, post_period[1], post_period[length(post_period)])
              
              
              return(new("LOOunitsMS", input = input, preps = preps, fits = fits, treated = c(as.character(input$names.and.numbers[1,1]), as.character(input$names.and.numbers[1,2]) ), treatment_time = treatment_time, PreRMSPE = RMSPES[[1]], PostRMSPE = RMSPES[[2]], RMSPEratio = RMSPES[[3]], CovBalances = Cov, ATEs = ATEs))  
            } #close units if loop
            
            #if this is a leave one out covaraites 
            if(type == "covariates"){
              
              preps <- LOOcovariatesMSPrep(input)
              
              fits <- plyr::llply(.data = preps, .fun = synth, ... , .parallel = parallel)
              names(fits) <- names(preps)
                RMSPES <- MultiSynthErrorRatios(preps, fits, treatment_time, input)
                Cov <- sapply(fits, function(synth){ return(synth$loss.w)})
                post_period <- input$tag$time.plot[input$tag$time.plot > treatment_time]
                ATEs <- MultiSynthATE(preps, fits, post_period[1], post_period[length(post_period)])
              
              
              return(new("LOOcovariatesMS", input = input, preps = preps, fits = fits, treated = c(as.character(input$names.and.numbers[1,1]), as.character(input$names.and.numbers[1,2]) ), treatment_time = treatment_time, PreRMSPE = RMSPES[[1]], PostRMSPE = RMSPES[[2]], RMSPEratio = RMSPES[[3]], CovBalances = Cov, ATEs = ATEs))
            } #close covariates if loop
          } #close function