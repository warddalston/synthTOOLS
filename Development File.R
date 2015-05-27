library(devtools)
library(roxygen2)

setwd("/Users/clockbob1/Documents/WashU 2nd Year/Causal Inference/Research Project/Code/synthTOOLS")

#create("synthTools")

current.code <- as.package("synthTools")
load_all(current.code)
document(current.code)
