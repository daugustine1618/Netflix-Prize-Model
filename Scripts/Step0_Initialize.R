
# Define names of packages to load for scripts. Not all are actually necessary.

Packages <- c("bigmemory", "broom", "caret", "compiler", "caTools", "data.table", "doParallel", "dplyr", "dslabs", "e1071", "fastAdaboost", "foreach", "formatR", "gam", "genefilter",
  "ggplot2", "ggrepel", "gridExtra", "HistData", "kernlab", "knitr", "Lahman", "lpSolve", "lubridate", "MASS", "matrixStats", "mvtnorm", "naivebayes", "parallel", "pdftools", "purrr",
  "randomForest", "ranger", "Rborist", "RColorBrewer", "recommenderlab", "recosystem", "reshape2", "ROSE", "rpart", "rpart.plot", "rtweet", "rvest", "scales", "snow", "stringr", "svMisc",
  "textdata", "tibble", "tidyr", "tidytext", "tidyverse", "tree")

# Download and install any packages not already installed and then load them.

for(p in Packages){
  if(!require(p, character.only = TRUE)){install.packages(p, character.only = TRUE, repos = "http://cran.us.r-project.org")}
  library(p, character.only = TRUE)
}

# Define a function that allows easy viewing of multiple figures simultaneously.

Show <- function(v){
  dev.new(noRStudioGD = TRUE)
  print(v)
}

# Define a function that prints a progress bar for time-consuming calculations.

ProgressBar <- function(i){
  cat("     Progress: [", strrep(c("|", " "), c(i, 100 - i)), "] ", ifelse(i != 100, paste(i, "%"), "Done!"), "\r", if(i == 100){c("\n", "\n")}, sep  = "")
  flush.console()
}

# Define a function that assigns a global scope to its input argument (to be used when utilizing parallel processing to speed up certain calculations).

make_global <- function(task_arg){arg <<- task_arg}

# Assign the number of logical processors to be used for parallel processing. For my computer, this is 20. The parallel processing code in this project is therefore based on this value.

logical_CPUs <- detectCores(logical = TRUE)

rm(Packages, p)
