# load libraries
library(readxl)
library(dplyr)


#
start_time <- Sys.time()
gc()
mem_before <- sum(gc()[, "used"])

# find files
xlsx_files <- list.files("datasets", pattern = "\\.xlsx$", full.names = TRUE)

# create main output folder if it doesn't exist
if (!dir.exists("SpeedNe_input")) {
  dir.create("SpeedNe_input")
}

# create SpeedNe_input file
for (file_path in xlsx_files) {
  # load file
  sheet <- "welcomeR"
  file_name <- tools::file_path_sans_ext(basename(file_path))
  Speed_Ne_file <-  as.data.frame(read_xlsx(file_path, sheet = sheet))
  
  
  # If there are "NA"s in the Speed_Ne_file they have to be substituted by 0for (locus in names(data_frame_without_information)) { # all columnnames
  for (locus in names(Speed_Ne_file)) { # all column names
    for (i in 1:nrow(Speed_Ne_file)) { # index every row
      if (is.na(Speed_Ne_file[i, locus])) {
        Speed_Ne_file[i, locus] <- -9  # convert <NA> into 0!
      } else if (grepl("NA", Speed_Ne_file[i , locus])) {
        Speed_Ne_file[i, locus] <- -9 # convert "NA" into 0!
      } else if (Speed_Ne_file[i, locus] == 0) {
        Speed_Ne_file[i, locus] <- -9
      }
    }
  }
  
  
  ## Create files containing tables for every site 
  # Remember names of the sites. This script only works if names of the sites are in the second column!
  sites <- unique(Speed_Ne_file[, 2])
  # create an empty list for the data of each site 
  # the list has the length as the number of sites existing
  data_sites_list <- vector("list", length(sites)) 
  # every list element is named after a site
  names(data_sites_list) <- sites
  # add data of the sites to the empty list and delete individuals with to much missing data
  for (site in sites) {
    does_the_row_contain_site <- Speed_Ne_file[, 2] == site
    data_site <- Speed_Ne_file[does_the_row_contain_site, ] # load the data
    rows <- nrow(data_site) # number of individuals
    # every row is TRUE at first (means less missing data than 20%), until proven wrong
    rows_to_keep <- rep(TRUE, rows)
    # check all rows/individuals for missing data
    for (row in 1:rows) {
      #calculate the percentage of missing data for every individual
      columns <- ncol(data_site) # number of columns
      count_missing_data <- sum(data_site[row, ] == -9) 
      percentage <- count_missing_data / columns
      # if one individual has more missing data than the threshold make a note
      if (percentage > 0.2) {
        rows_to_keep[row] <- FALSE
      }
    }
    # only keep sites with missing values below the threshold
    data_site <- data_site[rows_to_keep, ]
    # only save the site if there are still more than 30 individuals left or a different threshold set at the beginning
    rows <- nrow(data_site) # number of individuals left in the data set for that site
    if (rows > 30){
      data_sites_list[[site]] <- data_site
    }
    # create folder for this Excel file if it doesn't exist
    file_folder <- file.path("SpeedNe_input", file_name)
    if (!dir.exists(file_folder)) {
      dir.create(file_folder, recursive = TRUE)
    }
    # save the data of each site as a single .csv- document
    site_name <- paste(site, ".csv", sep = "")
    write.csv(data_site[, -c(1, 2)], file = file.path("SpeedNe_input", file_name, site_name), row.names = FALSE)
  }
}



# The files contain column names that are confusing for Matlab and must be deleted.
# Load the input files back into R
input_files <- list.files("SpeedNe_input", pattern = "\\.csv$", full.names = TRUE)
# Loop to delete the first line and overwrite the files
for (file in input_files) {
  text <- readLines(file)
  text <- gsub("\"", "", text) #remove ""
  text <- text[-1]
  writeLines(text, file)
}

# this did not work, column names were removed manually and " was removed manually if numbers were treated as characters, but was not time tracked

mem_after <- sum(gc()[, "used"])
end_time <- Sys.time()
end_time - start_time
mem_after - mem_before