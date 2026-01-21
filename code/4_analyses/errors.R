library(dplyr)

## Load data
speedne <- read.csv("results/SpeedNe/processed/summary.csv")
neest   <- read.csv("results/NeEstimator/summary_Ne.csv")

# # clean data
# # show sites which are in speedne but not in neest
# anti_join(speedne , neest, by = c("site", "dataset")) %>%
#   select(site, dataset) %>%
#   distinct()

# add marker
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
neest <- neest %>%
  mutate(
    marker = case_when(
      dataset %in% microsatellite_data_sets ~ "microsatellite",
      dataset %in% SNP_data_sets             ~ "SNP"
    )
  )
speedne <- speedne %>%
  mutate(
    marker = case_when(
      dataset %in% microsatellite_data_sets ~ "microsatellite",
      dataset %in% SNP_data_sets             ~ "SNP"
    )
  )

# # how many of the Ne calculations failed in %
# # no Ne is infinite
# mean(is.na(neest$Ne) | neest$Ne < 0) * 100
# mean(is.na(speedne$Ne) | speedne$Ne < 0) * 100

# in how many calculations failed in % per dataset?
speed_errors_percent <- speedne %>%
  group_by(marker, dataset) %>%
  summarise(
    total = n(),
    failed = sum(is.na(Ne) | Ne < 0 | is.infinite(Ne)),
    failed_percent = 100 * failed / total,
    .groups = "drop"
  )
# mean and sd per marker type
speed_separate <- speed_errors_percent %>%
  group_by(marker) %>%
  summarise(
    avg_failed_percent = mean(failed_percent),
    sd_failed_percent  = sd(failed_percent),
    .groups = "drop"
  )
speed_together <- speed_errors_percent %>%
  summarise(
    marker = "all",
    avg_failed_percent = mean(failed_percent),
    sd_failed_percent = sd(failed_percent)
  )
speed_mean_sd <- bind_rows(speed_separate, speed_together)


# same for Neestimator
neest_errors_percent <- neest %>%
  group_by(marker, dataset) %>%
  summarise(
    total = n(),
    failed = sum(is.na(Ne) | Ne < 0 | is.infinite(Ne)),
    failed_percent = 100 * failed / total,
    .groups = "drop"
  )
neest_separate <- neest_errors_percent %>%
  group_by(marker) %>%
  summarise(
    avg_failed_percent = mean(failed_percent),
    sd_failed_percent  = sd(failed_percent),
    .groups = "drop"
  )
neest_together <- neest_errors_percent %>%
  summarise(
    marker = "all",
    avg_failed_percent = mean(failed_percent),
    sd_failed_percent = sd(failed_percent)
  )
neest_mean_sd <- bind_rows(neest_separate, neest_together)

# combine all information including programs
speed_mean_sd2 <- speed_mean_sd %>%
  mutate(source = "speedne")
neest_mean_sd2 <- neest_mean_sd %>%
  mutate(source = "neest")
errors_mean_sd <- bind_rows(speed_mean_sd2, neest_mean_sd2)

write.csv(errors_mean_sd, "results/errors.csv")
