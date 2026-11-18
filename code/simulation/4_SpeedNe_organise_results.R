# Summarize results of SpeedNe

# Search all .txt files in SpeedNe_commandline_output
SpeedNe_files <- list.files(
  path = "results/simulate/SpeedNe_commandline_output",
  pattern = "\\.txt$",
  full.names = TRUE,
  recursive = TRUE
)

# Extract site and dataset names
site    <- sub("\\.txt$", "", basename(SpeedNe_files))
dataset <- basename(dirname(SpeedNe_files))

# Prepare summary dataframe
summary <- data.frame(
  site     = character(),
  dataset  = character(),
  Ne       = numeric(),
  lower_CI = numeric(),
  upper_CI = numeric(),
  stringsAsFactors = FALSE
)

# Extract Ne results from all files
for (i in seq_along(SpeedNe_files)) {
  
  SpeedNe_txt <- readLines(SpeedNe_files[i])
  
  # Find Ne
  # Estimates with Weir's (1979) S/(S-1) weighting of r^2 for bias correction.
  # r^2_{delta} AFT
  # percentile confidence interval
  # jackknifing over loci
  line_start_jackknife_over_loci <- grep( "delete-one jackknifing over loci", SpeedNe_txt,ignore.case = TRUE)
  if (length(line_start_jackknife_over_loci) == 1 &&  length(SpeedNe_txt) >= line_start_jackknife_over_loci + 64) {
    result_txt <- SpeedNe_txt[line_start_jackknife_over_loci + 64]
    
    results <- unlist(
      regmatches(
        result_txt,
        gregexpr(
          "-?\\d+\\.?\\d*|infinity",
          result_txt,
          ignore.case = TRUE
        )
      )
    )
    
    results <- unname(sapply(results, function(x) {
      if (tolower(x) == "infinity") Inf else as.numeric(x)
    }))
    
    if (length(results) >= 3) {
      Ne       <- results[1]
      lower_CI <- results[2]
      upper_CI <- results[3]
    } else {
      Ne       <- NA
      lower_CI <- NA
      upper_CI <- NA
    }
    
  } else {
    Ne       <- NA
    lower_CI <- NA
    upper_CI <- NA
  }
  
  # Add results to summary table
  summary <- rbind(
    summary,
    data.frame(
      site     = site[i],
      dataset  = dataset[i],
      Ne       = round(Ne, 1),
      lower_CI = round(lower_CI, 1),
      upper_CI = round(upper_CI, 1)
    )
  )
}

# save summary
write.csv(summary, "results/simulate/SpeedNe.csv")