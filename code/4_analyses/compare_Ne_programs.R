## Load data
# SpeedNe
files_speedne <- list.files("results/SpeedNe/processed",  pattern = "\\.csv$",  full.names = TRUE)
speedne_list <- lapply(files_speedne, read.csv)
names(speedne_list) <- basename(files_speedne)
# NeEstimator
files_neest <- list.files(  "results/NeEstimator/Ne",  pattern = "\\.csv$",  full.names = TRUE)
neest_list <- lapply(files_neest, read.csv)
names(neest_list) <- basename(files_neest)

# how many of the datasets were not possible to be calculated without an error for at least one of the populations
# SpeedNe 
common_files   <- intersect(names(speedne_list), names(neest_list))
row_comparison <- sapply(common_files, function(f) {nrow(speedne_list[[f]]) < nrow(neest_list[[f]])})
SN_success     <- sum(row_comparison) + 1 # for Pyura chilensis
SN_error_rate  <- (30-SN_success)/30 *100

# NeEstimator
NE_error_rate <- (30-length(neest_list))/30 *100

