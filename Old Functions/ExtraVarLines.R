
ExtraVar_lines <- function(ExtraVar1, ExtraVar2 = NA, originalVar, treatmentYear, data, case, caseVar, yearVar, yearInt = 5, ...){
  
  print(c(data[data$ctr_n %in% case & data$yr == (treatmentYear - yearInt), originalVar], data[data$ctr_n %in% case & data$yr >= (treatmentYear), ExtraVar1]))
  print(data[data$ctr_n %in% case & data$yr >= (treatmentYear - yearInt), "yr"])
  
  lines( x = data[data$ctr_n %in% case & data$yr >= (treatmentYear - yearInt), "yr"], y = c(data[data$ctr_n %in% case & data$yr == (treatmentYear - yearInt), originalVar], data[data$ctr_n %in% case & data$yr >= (treatmentYear), ExtraVar1]),  ... )
  
  if(!is.na(ExtraVar2)){
    lines( x = data[data$ctr_n %in% case & data$yr >= (treatmentYear - yearInt), "yr"], y = c(data[data$ctr_n %in% case & data$yr == (treatmentYear - yearInt), originalVar], data[data$ctr_n %in% case & data$yr >= (treatmentYear), ExtraVar2]), ... )
  }
}
