MultiSynthPrep <- function(input, type = "placebos"){
  if(type == "placebos"){
    
  #create list of controls  
  units <- c(input$tag$treatment.identifier,input$tag$controls.identifier)
  
  out <-   sapply(units, function(x){
    
    #for the original, just return that
    if( x == input$tag$treatment.identifier){
      return(input)
    } 
    
    #for the others, create a placebo dataprep object
      temp <- list(
        X0 = cbind(input$X0[ ,!colnames(input$X0) == x ], input$X1),
        X1 = matrix(input$X0[ ,colnames(input$X0) == x ], dimnames = list(rownames(input$X0), as.character(x))),
        Z0 = cbind(input$Z0[ ,!colnames(input$Z0) == x ], input$Z1),
        Z1 = matrix(input$Z0[ ,colnames(input$Z0) == x ], dimnames = list(rownames(input$Z0), as.character(x))),
        Y0plot = cbind(input$Y0plot[ ,!colnames(input$Y0plot) == x ], input$Y1plot),
        Y1plot = matrix(input$Y0plot[ ,colnames(input$Y0plot) == x ], dimnames = list(rownames(input$Y0plot), as.character(x))),
        names.and.numbers = input$names.and.numbers,
        tag = list(foo = input$tag$foo,
                   predictors = input$tag$predictors,
                   predictors.op = input$tag$predictors.op,
                   special.predictors = input$tag$special.predictors,
                   dependent = input$tag$dependent,
                   unit.variable = input$tag$unit.variable,
                   time.variable = input$tag$time.variable,
                   treatment.identifier = x,
                   controls.identifier = c(input$tag$controls.identifier[input$tag$controls.identifier != x], input$tag$treatment.identifier),
                   time.predictors.prior = input$tag$time.predictors.prior,
                   time.optimize.ssr = input$tag$time.optimize.ssr,
                   time.plot = input$tag$time.plot,
                   unit.names.variable = input$tag$unit.names.variable
        ) #close tag list
      ) #close temp list
      return(temp) #for the sapply function
      }, simplify = FALSE)
  
  #give objects of this big list helpful names!
  names(out) <- as.character(input$names.and.numbers[,1])
  class(out) <- "Placebos"
  return(out)
  
  } else if(type == "units"){
    
    #create list of units  
    units <- c(input$tag$treatment.identifier,input$tag$controls.identifier)
    
    out <-   sapply(units, function(x){
      
      #for the original, just return that
      if( x == input$tag$treatment.identifier){
        return(input)
      } 
      
      #for the others, create a placebo dataprep object
      temp <- list(
        X0 = input$X0[ ,!colnames(input$X0) == x ],
        X1 = input$X1,
        Z0 = input$Z0[ ,!colnames(input$Z0) == x ],
        Z1 = input$Z1,
        Y0plot = input$Y0plot[ ,!colnames(input$Y0plot) == x ],
        Y1plot = input$Y1plot,
        names.and.numbers = input$names.and.numbers[!input$names.and.numbers[,2] ==  x,],
        tag = list(foo = input$tag$foo,
                   predictors = input$tag$predictors,
                   predictors.op = input$tag$predictors.op,
                   special.predictors = input$tag$special.predictors,
                   dependent = input$tag$dependent,
                   unit.variable = input$tag$unit.variable,
                   time.variable = input$tag$time.variable,
                   treatment.identifier = x,
                   controls.identifier = input$tag$controls.identifier[input$tag$controls.identifier != x],
                   time.predictors.prior = input$tag$time.predictors.prior,
                   time.optimize.ssr = input$tag$time.optimize.ssr,
                   time.plot = input$tag$time.plot,
                   unit.names.variable = input$tag$unit.names.variable
        ) #close tag list
      ) #close temp list
      return(temp) #for the sapply function
    }, simplify = FALSE)
    
    #give objects of this big list helpful names!
    names(out) <- c("Full Donor Pool", paste("Minus",as.character(input$names.and.numbers[-1,1]), sep = " "))
    class(out) <- "Leave-One-Out Units"
    return(out)
    
  } else if(type == "covariates"){
    #create list of units  
    units <- c("FULL_SET", rownames(input$X0))
    
    out <-   sapply(units, function(x){
      
      #for the original, just return that
      if( x == "FULL_SET"){
        return(input)
      } 
      
      #for the others, create a placebo dataprep object
      temp <- list(
        X0 = input$X0[ !rownames(input$X0) == x ,],
        X1 = matrix(input$X1[ !rownames(input$X1) == x ,], dimnames = list(rownames(input$X1)[!rownames(input$X1) == x], colnames(input$X1))),
        Z0 = input$Z0,
        Z1 = input$Z1,
        Y0plot = input$Y0plot,
        Y1plot = input$Y1plot,
        names.and.numbers = input$names.and.numbers,
        tag = list(foo = input$tag$foo,
                   predictors = input$tag$predictors,
                   predictors.op = input$tag$predictors.op,
                   special.predictors = input$tag$special.predictors,
                   dependent = input$tag$dependent,
                   unit.variable = input$tag$unit.variable,
                   time.variable = input$tag$time.variable,
                   treatment.identifier = x,
                   controls.identifier = input$tag$controls.identifier[input$tag$controls.identifier != x],
                   time.predictors.prior = input$tag$time.predictors.prior,
                   time.optimize.ssr = input$tag$time.optimize.ssr,
                   time.plot = input$tag$time.plot,
                   unit.names.variable = input$tag$unit.names.variable
        ) #close tag list
      ) #close temp list
      return(temp) #for the sapply function
    }, simplify = FALSE)
    
    #give objects of this big list helpful names!
    names(out) <- c("Full Covariate Set", paste("Minus",units[-1], sep = " "))
    class(out) <- "Leave-One-Out Covariates"
    return(out)
  }
}