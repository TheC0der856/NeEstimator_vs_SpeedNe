# create a table for runtime and memory use (NeEstimator)

NeEstimator_info <- readLines("results/NeEstimator/runtime_memory.txt")

# seconds of calculation
wall <- NeEstimator_info[grepl("^Job Wall-clock time:", NeEstimator_info)]
time_str <- sub(".*time: ", "", wall)
to_seconds <- function(t) {
  hms <- as.integer(strsplit(t, ":")[[1]])
  hms[1]*3600 + hms[2]*60 + hms[3]
}
seconds <- sapply(time_str, to_seconds)

# memory utliized
mem <- NeEstimator_info[grepl("^Memory Utilized:", NeEstimator_info)]
memory_mb <- as.numeric(sub(".*: ([0-9.]+) MB", "\\1", mem))

# job names
csv_files <- list.files(
  "results/NeEstimator/Ne",
  pattern = "\\.(csv|err)$",
  full.names = FALSE
)

# create df 
df <- data.frame(
  file = csv_files,
  seconds = seconds,
  memory_mb = memory_mb,
  row.names = NULL
)

df$file <- sub("\\.csv$", "", df$file)
df$file <- sub("\\.err$", "", df$file)

write.csv(df, "results/NeEstimator/runtime_memory.csv")
