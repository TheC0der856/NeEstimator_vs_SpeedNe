library(dplyr)

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
row_comparison <- sapply(common_files, function(f) {nrow(speedne_list[[f]]) == nrow(neest_list[[f]])})
SN_success     <- sum(row_comparison) + 1 # for Pyura chilensis
SN_error_rate  <- (30-SN_success)/30 *100

# NeEstimator
neest_common_df <- bind_rows(
  lapply(common_files, function(f) {
    neest_list[[f]] %>%
      mutate(
        across(where(is.character), ~ suppressWarnings(as.numeric(.))),
        dataset = f
      )
  })
)
datasets_problematic_Ne <- neest_common_df %>%
  group_by(dataset) %>%
  summarise(
    has_Ne_NA = any(is.na(Ne)),
    has_Ne_negative = any(Ne < 0, na.rm = TRUE)
  ) %>%
  filter(has_Ne_NA | has_Ne_negative)

NE_error_rate <- (30-(length(neest_list)-nrow(datasets_problematic_Ne)))/30 *100

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
# SpeedNe
# SNPS success:  0%
# micro success:  Ambystoma_bishopi, Cercidiphyllum_japonicum, Cystoseira_amentaceae, 
#                 Mytilus_galloprovincialis, Panthera_leo, Syncerus_caffer, Pyura_chilensis
#7/15*100 # = 47%
# error rate:
# SNPs: 100%
# micro: 53%

# NeEstimator
# failed SNPs: Argiope_bruennichi, Atriophallophorus_winterbourni, Entosphenus_tridentatus,
# microsatellites: Pyura_chilensis, Avicennia_marina, Carcinus_meanas, Cymodocea_nodosa, Mytilus_galloprovincialis, Posidonia_oceanica
# SNPs: 3 von 15
# micro: 6 von 15
#6/15*100 # = 40%
#3/15 *100 # =20%

# are the results comparable?
speedne_df <- bind_rows(
  lapply(names(speedne_list), function(f) {
    speedne_list[[f]] %>%
      mutate(dataset = f)
  })
)
neest_df <- bind_rows(
  lapply(names(neest_list), function(f) {
    neest_list[[f]] %>%
      mutate(
        across(-Pop, ~ suppressWarnings(as.numeric(.))),
        dataset = f
      )
  })
)
neest_df <- neest_df %>%
  rename(site = Pop)

compare_ne <- neest_df %>%
  select(dataset, site, Ne_neest = Ne) %>%
  inner_join(
    speedne_df %>%
      select(dataset, site, Ne_speedne = Ne),
    by = c("dataset", "site")
  )
compare_ne <- compare_ne %>%
  filter(!is.na(Ne_neest), Ne_neest >= 0)

compare_ne %>%
  summarise(
    median_Ne_neest = median(Ne_neest, na.rm = TRUE),
    median_Ne_speedne = median(Ne_speedne, na.rm = TRUE)
  )
#median_Ne_neest median_Ne_speedne
#1            30.6          31.63773
# median, weil schiefe verteilung.

# comparison between CI
# parametric CI is bad, because of the distribution of the values. 
# ein Jackknife-Konfidenzintervall und ein Bootstrap-Percentile-Intervall sind nicht direkt miteinander vergleichbar, 
# auch wenn beide „nicht-parametrisch“ sind
# Percentile-Intervall more exact more calculation intensive, but more recommended than jacknife