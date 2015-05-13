MultiSynthPrep <- function(input, type = "placebos"){
  
  #what type of multi synth is this?
  if(type == "placebos"){
    #create list of controls  
    units <- c(input$tag$treatment.identifier,input$tag$controls.identifier)
    
    #create the prep objects
    out <-   alply(.data = units, .margins = 1, .fun = function(x){
      
      #for the original, just return that
      if( x == input$tag$treatment.identifier){
        return(input[1:7])
      } 
      
      #for the others, create a placebo dataprep object
      temp <- list(
        X0 = cbind(input$X0[ ,!colnames(input$X0) == x ], input$X1),
        X1 = matrix(input$X0[ ,colnames(input$X0) == x ], 
                    dimnames = list(rownames(input$X0), as.character(x))),
        Z0 = cbind(input$Z0[ ,!colnames(input$Z0) == x ], input$Z1),
        Z1 = matrix(input$Z0[ ,colnames(input$Z0) == x ], 
                    dimnames = list(rownames(input$Z0), as.character(x))),
        Y0plot = cbind(input$Y0plot[ ,!colnames(input$Y0plot) == x ], input$Y1plot),
        Y1plot = matrix(input$Y0plot[ ,colnames(input$Y0plot) == x ], 
                        dimnames = list(rownames(input$Y0plot), as.character(x))),
        names.and.numbers = input$names.and.numbers
      ) #close temp list
      return(temp) #for the alply function
    })
    
    #give objects of this big list helpful names!
    names(out) <- as.character(input$names.and.numbers[,1])
    
    #close the if for placebo multisynth, start a new one for leave-one-out units
  } else if(type == "units"){
    #create list of units  
    units <- c(input$tag$treatment.identifier,input$tag$controls.identifier)
    
    out <-   alply(.data = units, .margins = 1, .fun = function(x){
      
      #for the original, just return that
      if( x == input$tag$treatment.identifier){
        return(input[1:7])
      } 
      
      #for the others, create a placebo dataprep object
      temp <- list(
        X0 = input$X0[ ,!colnames(input$X0) == x ],
        X1 = input$X1,
        Z0 = input$Z0[ ,!colnames(input$Z0) == x ],
        Z1 = input$Z1,
        Y0plot = input$Y0plot[ ,!colnames(input$Y0plot) == x ],
        Y1plot = input$Y1plot,
        names.and.numbers = input$names.and.numbers[!input$names.and.numbers[,2] ==  x,]
      ) #close temp list
      return(temp) #for the sapply function
    })
    
    #give objects of this big list helpful names!
    names(out) <- c("Full Donor Pool", 
                    paste("Minus",as.character(input$names.and.numbers[-1,1]), sep = " "))
    
    #close if loop for units, open final one for covariates
  } else if(type == "covariates"){
    units <- c("FULL_SET", rownames(input$X0))
    
    out <-   alply(.data = units, .margins = 1, .fun = function(x){
      
      #for the original, just return that
      if( x == "FULL_SET"){
        return(input[1:7])
      } 
      
      #for the others, create a placebo dataprep object
      temp <- list(
        X0 = input$X0[ !rownames(input$X0) == x ,],
        X1 = matrix(input$X1[ !rownames(input$X1) == x ,], 
                    dimnames = list(rownames(input$X1)[!rownames(input$X1) == x], colnames(input$X1))),
        Z0 = input$Z0,
        Z1 = input$Z1,
        Y0plot = input$Y0plot,
        Y1plot = input$Y1plot,
        names.and.numbers = input$names.and.numbers
      ) #close temp list
      return(temp) #for the alply function
    })
    
    #give objects of this big list helpful names!
    names(out) <- c("Full Covariate Set", paste("Minus",units[-1], sep = " "))
  } #close final if loop
  return(out)
} #close function 


setClass(Class="MultiSynth", 
         slots = list(
           input = "list",
           preps = "list",
           fits = "list",
           type = "character",
           treated = "character",
           treatment_time = "numeric"
         ),
         prototype = prototype(
           input = list(),
           preps = list(),
           fits = list(),
           type = character(),
           treated = character(),
           treatment_time = numeric()
         )
)

setMethod("initialize", "MultiSynth", 
          function(.Object, input = list(), preps = list(), fits = list(), type = character(), treated = character(), treatment_time = numeric()){ 
            #these are the default values for an object of class BMA.
            
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
            .Object@type <- type
            .Object@treated <- treated
            .Object@treatment_time <- treatment_time
            .Object
          }
)

