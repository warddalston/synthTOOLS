library(devtools)
library(roxygen2)

setwd("/Users/clockbob1/Documents/WashU 2nd Year/Causal Inference/Research Project/Code/synthTOOLS")

#create("synthTools")

current.code <- as.package("synthTools")
load_all(current.code)
document(current.code)

build(current.code, path = getwd())

install(current.code, local = TRUE)
check(current.code)

path_to_file <- "~/Documents/WashU 2nd Year/Causal Inference/Research Project/Code/synthTOOLS/synthTools_0.0.1.tar.gz"
install.packages(path_to_file, repos = NULL, type = "source")
