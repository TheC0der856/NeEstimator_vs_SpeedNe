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

# calculate the average 
NeEstimator_time <- sum(NeEstimator$sec2compare)/30
SpeedNe_time     <- sum(as.numeric(SpeedNe$seconds), na.rm = TRUE)/30 # I decided not to take the time into account that it took to structure input and output of the programm (running the script)

# calculate memory
NeEstimator_mem <- sum(NeEstimator$memory_mb)/30
SpeedNe_mem     <- sum(as.numeric(SpeedNe$memory_mb), na.rm = TRUE)/30

# test significance of difference
# robust, also when data is not normally distributed
# two sided because there is no preassumption which program would be faster, just testing if there is a difference or not

# prepare speed-ne time: remove errors
sp_time <- SpeedNe$seconds
sp_time[sp_time == "error"] <- NA
sp_time <- gsub(" ", "", sp_time)
sp_time <- as.numeric(sp_time)
sp_time <- sp_time[!is.na(sp_time)]


time_test <- wilcox.test(NeEstimator$sec2compare,
                         sp_time, 
                         alternative = "two.sided")

#Wilcoxon rank sum test with continuity correction
#data:  ne_time and sp_time
#W = 1, p-value = 7.066e-10
#alternative hypothesis: true location shift is not equal to 0
# p is significant -> p < 0.001 (3 stern)

# prepare SpeedNe memory: remove errors
sp_mem <- SpeedNe$memory_mb
sp_mem[sp_mem == "error"] <- NA
sp_mem <- as.numeric(sp_mem)
sp_mem <- sp_mem[!is.na(sp_mem)]
ne_mem <- NeEstimator$memory_mb
ne_mem <- ne_mem[!is.na(ne_mem)]

mem_test <- wilcox.test(
  ne_mem,
  sp_mem,
  alternative = "two.sided",
  exact = FALSE   # vermeidet Warnung wegen Bindungen
)

mem_test
#Wilcoxon rank sum test with continuity correction
#data:  ne_mem and sp_mem
#W = 163, p-value = 0.001125
#alternative hypothesis: true location shift is not equal to 0

#3stern p < 0.001, ** p < 0.01, * p < 0.05, this p is significant one star

# SNPs vs microsatellites
microsatellite_data_sets <- c("Ambystoma_bishopi", 
                              "Avicennia_marina", 
                              "Cameraria_ohridella", 
                              "Carcinus_meanas", 
                              "Cercidiphyllum_japonicum", 
                              "Cymodocea_nodosa", 
                              "Cystoseira_amentaceae",
                              "Euphydryas_aurina" ,
                              "Mytilus_galloprovincialis",
                              "Pagophila_eburnea", 
                              "Panthera_leo", 
                              "Posidonia_oceanica",
                              "Pyura_chilensis",
                              "Syncerus_caffer",
                              "Varroa_jacobsoni")
SNP_data_sets <- c("Abies_alba", 
                   "Argiope_bruennichi", 
                   "Atriophallophorus_winterbourni", 
                   "Dracocephalum_ruyschiana",
                   "Entosphenus_tridentatus",
                   "Frangula_alnus",
                   "Gadus_morhua", 
                   "Melitaea_cinxia",
                   "Nilparvata_lugens",
                   "Oncorhynchus_mykiss",
                   "Oncorhynchus_tshawytscha",
                   "Physeter_macrocephalus",
                   "Pinus_halepensis",
                   "Salmo_trutta",
                   "Tectona_grandis")
NeEstimator$SNP_vs_micro <- ifelse(
  NeEstimator$file %in% SNP_data_sets, "SNP",
  ifelse(NeEstimator$file %in% microsatellite_data_sets, "microsatellite", NA)
)
SpeedNe$SNP_vs_micro <- ifelse(
  SpeedNe$file %in% SNP_data_sets, "SNP",
  ifelse(SpeedNe$file %in% microsatellite_data_sets, "microsatellite", NA)
)


# SNPs time and memory
# number datasets
n_snp <- sum(NeEstimator$SNP_vs_micro == "SNP", na.rm = TRUE)
# time
NeEstimator_time <- sum(NeEstimator$sec2compare[NeEstimator$SNP_vs_micro == "SNP"],
                        na.rm = TRUE) / n_snp
SpeedNe_time <- sum(as.numeric(SpeedNe$seconds[NeEstimator$SNP_vs_micro == "SNP"]),
                    na.rm = TRUE) / n_snp

# memory
NeEstimator_mem <- sum(NeEstimator$memory_mb[NeEstimator$SNP_vs_micro == "SNP"],
                       na.rm = TRUE) / n_snp
SpeedNe_mem <- sum(as.numeric(SpeedNe$memory_mb[NeEstimator$SNP_vs_micro == "SNP"]),
                   na.rm = TRUE) / n_snp


# microsatellites
# number datasets
n_snp <- sum(NeEstimator$SNP_vs_micro == "microsatellite", na.rm = TRUE)
# time
NeEstimator_time <- sum(NeEstimator$sec2compare[NeEstimator$SNP_vs_micro == "microsatellite"],
                        na.rm = TRUE) / n_snp
SpeedNe_time <- sum(as.numeric(SpeedNe$seconds[NeEstimator$SNP_vs_micro == "microsatellite"]),
                    na.rm = TRUE) / n_snp

# memory
NeEstimator_mem <- sum(NeEstimator$memory_mb[NeEstimator$SNP_vs_micro == "microsatellite"],
                       na.rm = TRUE) / n_snp
SpeedNe_mem <- sum(as.numeric(SpeedNe$memory_mb[NeEstimator$SNP_vs_micro == "microsatellite"]),
                   na.rm = TRUE) / n_snp
