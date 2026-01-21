# Summarize results of SpeedNe

# file names
SpeedNe_files   <- list.files(path = "results/SpeedNe/output/", pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
site_name       <- sub("\\.txt$", "", basename(SpeedNe_files))
dataset_name    <- basename(dirname(SpeedNe_files))

# prepare data frame for result summary
summary <- data.frame(
  site_name    = character(),
  dataset_name = character(),
  Ne           = numeric(),
  lower_CI     = numeric(),
  upper_CI     = numeric(),
  stringsAsFactors = FALSE
)

# search Ne result and add it to the dataframe
for (i in seq_along(SpeedNe_files)) {
  SpeedNe_txt <- readLines(SpeedNe_files[i])
  
  # find line Ne 
  line_start_jackknife_over_loci <- grep("delete-one jackknifing over loci", SpeedNe_txt, ignore.case = TRUE)
  # Estimates with Weir's (1979) S/(S-1) weighting of r^2 for bias correction.
  # r^2_{delta} AFT
  # percentile confidence interval
  result_txt <- SpeedNe_txt[line_start_jackknife_over_loci + 64]
  
  if (length(result_txt) == 1) {
    results <- unlist(
        regmatches(
          result_txt,
          gregexpr("-?\\d+\\.?\\d*|infinity", result_txt, ignore.case = TRUE)
      )
    )
    results <- unname(sapply(results, function(x) {
      if (tolower(x) == "infinity") Inf else as.numeric(x)
    }))
    Ne       <- results[1]
    lower_CI <- results[2]
    upper_CI   <- results[3]
  } else {
    Ne       <- NA
    lower_CI <- NA
    upper_CI   <- NA
  }
  # add info to df
  summary <- rbind(
    summary,
    data.frame(
      site_name    = site_name[i],
      dataset_name = dataset_name[i],
      Ne           = round(Ne, 2),
      lower_CI     = round(lower_CI, 2),
      upper_CI     = round(upper_CI, 2),
      stringsAsFactors = FALSE
    )
  )
}

# save summary
write.csv(summary, "results/SpeedNe/processed/summary.csv")