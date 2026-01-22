# compare time and memory

# load data
NeEstimator <- read.csv("results/NeEstimator/runtime_memory.csv") # seconds is the whole calculation, memory was measured with random controll on task manager, but before that was also measured with sh code with server and comparable
SpeedNe     <- read.csv("results/SpeedNe/timing_memory/2_calculation.csv", sep = ";", stringsAsFactors = FALSE) # seconds is the average per single calculation, memory is from the task manager

# seconds were measured differently,
# in SpeedNe we had to calcualte the average per single calculation because some calculations failed an those need 
# different calculation times, depending when the error occurred.
# so I had to reduce the seconds of NeEStimator to seconds per pop
path  <- "results/NeEstimator/Ne/"
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
rows  <- sapply(files, function(f) {nrow(read.csv(f))})
NeEstimator$sec2compare <- NeEstimator$seconds/rows


# add marker information
microsatellite_data_sets <- c(
  "Ambystoma_bishopi","Avicennia_marina","Cameraria_ohridella",
  "Carcinus_meanas","Cercidiphyllum_japonicum","Cymodocea_nodosa",
  "Cystoseira_amentaceae","Euphydryas_aurina","Mytilus_galloprovincialis",
  "Pagophila_eburnea","Panthera_leo","Posidonia_oceanica",
  "Pyura_chilensis","Syncerus_caffer","Varroa_jacobsoni"
)
SNP_data_sets <- c(
  "Abies_alba","Argiope_bruennichi","Atriophallophorus_winterbourni",
  "Dracocephalum_ruyschiana","Entosphenus_tridentatus","Frangula_alnus",
  "Gadus_morhua","Melitaea_cinxia","Nilparvata_lugens","Oncorhynchus_mykiss",
  "Oncorhynchus_tshawytscha","Physeter_macrocephalus","Pinus_halepensis",
  "Salmo_trutta","Tectona_grandis"
)
add_marker_type <- function(df) {
  df %>%
    mutate(
      marker_type = case_when(
        file %in% SNP_data_sets ~ "SNP",
        file %in% microsatellite_data_sets ~ "microsatellite",
        TRUE ~ NA_character_
      )
    )
}
NeEstimator <- add_marker_type(NeEstimator)
SpeedNe     <- add_marker_type(SpeedNe)



# calculate the mean and sd
calc_mean_sd <- function(values, marker_idx = NULL) {
  if (!is.null(marker_idx)) values <- values[marker_idx]
  values <- as.numeric(values)
  c(mean = mean(values, na.rm = TRUE),
    sd   = sd(values, na.rm = TRUE))
}

markers <- c("all", "SNP", "microsatellite")
results <- list()
for (m in markers) {
  if (m == "all") {
    idx_ne <- NULL
    idx_sp <- NULL
  } else {
    idx_ne <- NeEstimator$marker_type == m
    idx_sp <- SpeedNe$marker_type     == m
  }
  results[[m]] <- data.frame(
    program       = c("NeEstimator", "SpeedNe"),
    time_mean_sec = c(calc_mean_sd(NeEstimator$sec2compare, idx_ne)["mean"],
                      calc_mean_sd(SpeedNe$seconds, idx_sp)["mean"]),
    time_sd_sec   = c(calc_mean_sd(NeEstimator$sec2compare, idx_ne)["sd"],
                      calc_mean_sd(SpeedNe$seconds, idx_sp)["sd"]),
    mem_mean_mb   = c(calc_mean_sd(NeEstimator$memory_mb, idx_ne)["mean"],
                      calc_mean_sd(SpeedNe$memory_mb, idx_sp)["mean"]),
    mem_sd_mb     = c(calc_mean_sd(NeEstimator$memory_mb, idx_ne)["sd"],
                      calc_mean_sd(SpeedNe$memory_mb, idx_sp)["sd"]),
    marker_type   = m
  )
}
performance_summary <- do.call(rbind, results)

# save results
write.csv(performance_summary, "results/time_memory.csv")



# test significance of difference
# robust, also when data is not normally distributed
# two sided because there is no preassumption which program would be faster, just testing if there is a difference or not

# clean: no NA and all values numeric
clean_numeric <- function(x) {
  x[x == "error"] <- NA
  as.numeric(gsub(" ", "", x))
}
ne_time <- na.omit(NeEstimator$sec2compare)
sp_time <- na.omit(clean_numeric(SpeedNe$seconds))
ne_mem  <- na.omit(NeEstimator$memory_mb)
sp_mem  <- na.omit(clean_numeric(SpeedNe$memory_mb))

# test
time_test <- wilcox.test(ne_time, sp_time, alternative = "two.sided") # p-value = 7.061e-10, three stars (p < 0.001)
mem_test  <- wilcox.test(ne_mem, sp_mem, alternative = "two.sided", exact = FALSE) # p-value = 0.001125, two stars (p < 0.01)
