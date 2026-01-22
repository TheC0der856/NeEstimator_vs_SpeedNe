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

# in how many calculations failed in % per dataset?
errors_percent <- function(df) {
  df %>%
    group_by(marker, dataset) %>%
    summarise(
      na_percent       = 100 * mean(is.na(Ne)),
      negative_percent = 100 * mean(Ne < 0, na.rm = TRUE),
      .groups = "drop"
    )
}
speed_errors_percent <- errors_percent(speedne)
neest_errors_percent <- errors_percent(neest)

# mean and sd markers separate
mean_sd_by_marker <- function(df) {
  df %>%
    group_by(marker) %>%
    summarise(
      mean_na_percent       = mean(na_percent),
      sd_na_percent         = sd(na_percent),
      mean_negative_percent = mean(negative_percent, na.rm = TRUE),
      sd_negative_percent   = sd(negative_percent, na.rm = TRUE),
      .groups = "drop"
    )
}
speed_separate <- mean_sd_by_marker(speed_errors_percent)
neest_separate <- mean_sd_by_marker(neest_errors_percent)

# mean and sd all markers
mean_sd_all <- function(df) {
  df %>%
    summarise(
      marker = "all",
      mean_na_percent       = mean(na_percent),
      sd_na_percent         = sd(na_percent),
      mean_negative_percent = mean(negative_percent, na.rm = TRUE),
      sd_negative_percent   = sd(negative_percent, na.rm = TRUE),
    )
}
speed_all <- mean_sd_all(speed_errors_percent)
neest_all <- mean_sd_all(neest_errors_percent)


# combine all information including programs
speed_mean_sd <- bind_rows(speed_separate, speed_all) %>%
  mutate(source = "speedne")
neest_mean_sd <- bind_rows(neest_separate, neest_all) %>%
  mutate(source = "neest")
errors_mean_sd <- bind_rows(speed_mean_sd, neest_mean_sd)

# rename
names(errors_mean_sd)[names(errors_mean_sd) == "source"] <- "program"
errors_mean_sd$program <- ifelse(errors_mean_sd$program == "speedne", "SpeedNe",
                          ifelse(errors_mean_sd$program == "neest", "NeEstimator",
                                errors_mean_sd$program))

# save
write.csv(errors_mean_sd, "results/errors.csv", row.names = FALSE)

# are differences significant?
wilcox.test(speed_errors_percent$na_percent,
            neest_errors_percent$na_percent, 
            paired = TRUE,
            alternative = "two.sided")
# p-value = 0.0009324, three stars (p < 0.001)
wilcox.test(speed_errors_percent$negative_percent,
            neest_errors_percent$negative_percent, 
            paired = TRUE,
            alternative = "two.sided")
# p-value = 0.2936, not significant