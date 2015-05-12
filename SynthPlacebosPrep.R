SynthPlacebosPrep <- function(data, predictors, dependent, unit.variable, time.variable, control.units, time.units.prior, time.units.ssr, unit.names, time.units.plot){
  
  lapply(control.units, function(case){
    dataprep(foo = data,
             predictors = predictors,
             dependent = dependent,
             unit.variable = unit.variable,
             time.variable = time.variable,
             treatment.identifier = case,
             controls.identifier = control.units[ !control.units == case],
             time.predictors.prior = time.units.prior,
             time.optimize.ssr = time.units.ssr,
             unit.names.variable = unit.names,
             time.plot = time.units.plot)
  })
}

MSPrep <- function(input, type = "placebos"){
  if(type == "placebos"){
    #do placebos prep
    
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
  
  } else if(type == "units"){
    print("Hello units")
    #do units prep
  } else if(type == "covariates"){
    #do covariates prep 
    print("Hello cov")
  }
}



input <- IT_five_year
str(IT_five_year)

out <- data.frame()
for(i in 1:length(IT_five_year$tag$foo)){
out[,i] <- IT_five_year$tag$foo[i]
}

# Covariates are rows, units are columns 
IT_five_year$X0

# row is a time period, units are columns
IT_five_year$Z0


temp <- list(
  X0 = IT_five_year$X0,
  X1 = IT_five_year$X0,
  Z0 = IT_five_year$Z0,
  Z1 = IT_five_year$Z1,
  Y0plot = IT_five_year$Y0plot,
  Y1plot = IT_five_year$Y1plot,
  names.and.numbers = IT_five_year$names.and.numbers,
  tag = IT_five_year$tag
)

newDP
IT_five_year
dataprep(foo = data,
         )


