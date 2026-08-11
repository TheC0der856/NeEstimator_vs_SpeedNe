# Install packages and load the libraries if they have not already been installed or loaded.
# For each package you will find a comment on what it is used for.

if(!require('readxl')) {       # read_xlsx() , load .xlsx-files
  install.packages('readxl')
}
if(!require('dplyr')) {       # %>% select() , simplifies working with tables
  install.packages('dplyr')
}
if(!require('RLDNe')) {       # enables using NeEstimatorV2.1 (Do et al. 2014) in R & calculate effective population sizes
  install.packages("devtools")   # install devtools to use the installation link to NeEstimator
  devtools::install_github(repo="zakrobinson/RLDNe")
}
