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
  # Find Ne
  line_start <- grep(
    "delete-one jackknifing over loci",
    SpeedNe_txt,
    ignore.case = TRUE
  )
  
  # First search: jackknifing over loci (+64 lines)
  if (length(line_start) == 1) {
    
    line_to_extract <- line_start + 64
    
  } else {
    
    # Alternative search: jackknife over r^2 (+65 lines)
    line_start <- grep(
      "Confidence intervals from a? delete-one jackknife over r",
      SpeedNe_txt,
      ignore.case = TRUE
    )
    
    if (length(line_start) == 1) {
      line_to_extract <- line_start + 65
    } else {
      line_to_extract <- NA_integer_
    }
  }
  
  # Extract Ne
  Ne <- NA_real_
  
  if (!is.na(line_to_extract) && line_to_extract <= length(SpeedNe_txt)) {
    
    result_txt <- SpeedNe_txt[line_to_extract]
    
    results <- regmatches(
      result_txt,
      gregexpr("-?\\d+\\.?\\d*|infinity", result_txt, ignore.case = TRUE)
    )[[1]]
    
    if (length(results) >= 1) {
      Ne <- if (tolower(results[1]) == "infinity") {
        Inf
      } else {
        as.numeric(results[1])
      }
    }
  }
  
  # Add results to summary table
  summary <- rbind(
    summary,
    data.frame(
      site     = site[i],
      dataset  = dataset[i],
      Ne       = round(Ne, 1)
    )
  )
}

# save summary
write.csv(summary, "results/simulate/SpeedNe.csv")