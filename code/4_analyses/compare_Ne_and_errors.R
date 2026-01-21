library(dplyr)

## Load data
speedne <- read.csv("results/SpeedNe/processed/summary.csv")
neest   <- read.csv("results/NeEstimator/summary_Ne.csv")

# clean data
# show sites which are in speedne but not in neest
anti_join(speedne, neest , by = c("site", "dataset")) %>%
  select(site, dataset) %>%
  distinct()

# Why?
# is Svalbard missing in NeEstimator?


# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Euphydryas_aurina") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Euphydryas_aurina") %>%
  pull(site)
# remove sites
speedne <- speedne %>%
  filter(!(site %in% setdiff(speedne_sites, neest_sites)))

# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Carcinus_meanas") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Carcinus_meanas") %>%
  pull(site)
# remove sites
speedne <- speedne %>%
  filter(!(site %in% setdiff(speedne_sites, neest_sites)))

# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Carcinus_meanas") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Carcinus_meanas") %>%
  pull(site)
# remove sites
speedne <- speedne %>%
  filter(!(site %in% setdiff(speedne_sites, neest_sites)))

# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Cymodocea_nodosa") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Cymodocea_nodosa") %>%
  pull(site)
neest <- neest %>%
  mutate(site = ifelse(site == "layaSjuan", "PlayaSjuan", site))
neest <- neest %>%
  mutate(site = ifelse(site == "EsBanc", "EsBlanc", site))
speedne <- speedne %>%
  mutate(site = ifelse(site == "LEsperanza", "Esperanza", site))

# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Entosphenus_tridentatus") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Entosphenus_tridentatus") %>%
  pull(site)
speedne_sites 
neest_sites
neest <- neest %>%
  mutate(site = ifelse(site == "15_Mile_Creek", "15_Miles_Creek", site))


# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Avicennia_marina") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Avicennia_marina") %>%
  pull(site)
speedne_sites
neest_sites # has additional X
# remove X
neest <- neest %>%
  mutate(site = ifelse(
    dataset == "Avicennia_marina",
    sub("^X", "", site),    # entfernt führendes "X"
    site                    # alle anderen unverändert
  ))

# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Tectona_grandis") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Tectona_grandis") %>%
  pull(site)
speedne_sites # has sites which do not have 30 individuals
neest_sites
# remove sites
speedne <- speedne %>%
  filter(!(site %in% setdiff(speedne_sites, neest_sites)))

# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Cameraria_ohridella") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Cameraria_ohridella") %>%
  pull(site)
# remove sites
speedne <- speedne %>%
  filter(!(site %in% setdiff(speedne_sites, neest_sites)))

# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Gadus_morhua") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Gadus_morhua") %>%
  pull(site)
# remove sites
speedne <- speedne %>%
  filter(!(site %in% setdiff(speedne_sites, neest_sites)))

# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Cystoseira_amentaceae") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Cystoseira_amentaceae") %>%
  pull(site)
# remove sites
speedne <- speedne %>%
  filter(!(site %in% setdiff(speedne_sites, neest_sites)))

# Why?
speedne_sites <- speedne %>%
  filter(dataset == "Pinus_halepensis") %>%
  pull(site)
neest_sites <- neest %>%
  filter(dataset == "Pinus_halepensis") %>%
  pull(site)
# use _ instead
speedne <- speedne %>%
  mutate(site = gsub("[\\. ]", "_", site))
neest <- neest %>%
  mutate(site = gsub("[\\. ]", "_", site))





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

