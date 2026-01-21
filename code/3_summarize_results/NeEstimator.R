# summarize NeEstimator
library(dplyr)
library(readxl)

# load NeEstimator files as list
files_neest <- list.files(  "results/NeEstimator/Ne",  pattern = "\\.csv$",  full.names = TRUE)
neest_list <- lapply(files_neest, read.csv)
names(neest_list) <- tools::file_path_sans_ext(basename(files_neest))

# create df
neest_df <- bind_rows(
  lapply(names(neest_list), function(f) {
    neest_list[[f]] %>%
      mutate(
        across(-Pop, ~ suppressWarnings(as.numeric(.))),
        dataset = f
      )
  })
)

# remove paramteric CI
neest_df <- neest_df %>%
  select(-contains("parametric"))

# rename 
neest_df <- neest_df %>%
  rename(
    site     = Pop,
    lower_CI = lower_jackknife_confidence_interval,
    upper_CI = upper_jackknife_confidence_interval
  )

###### correct site number and site names
# find correct names in the original datasets
datasets     <- list.files(  "datasets/", full.names = TRUE)
species_name <- tools::file_path_sans_ext(basename(datasets))
dataset_list <- lapply(datasets, read_xlsx)

unique_sites <- lapply(dataset_list, function(df) {
  tab <- table(df[[2]])
  names(tab[tab > 29])
})
names(unique_sites) <- species_name

# rename every site which was renamed with NeEstimator
neest_df <- neest_df %>%
  group_by(dataset) %>%
  mutate(
    site = {
      pop_idx <- which(grepl("^pop", site))
      if (length(pop_idx) > 0 && dataset[1] %in% names(unique_sites)) {
        site[pop_idx] <- unique_sites[[dataset[1]]][seq_along(pop_idx)]
      }
      site
    }
  ) %>%
  ungroup()

# add Pyura chilensis, which was removed because there was something strange with one locus
missing_rows <- data.frame(
  site = unique_sites[["Pyura_chilensis"]],
  Ne = NA_real_,
  lower_CI = NA_real_,
  upper_CI = NA_real_,
  dataset = "Pyura_chilensis",
  stringsAsFactors = FALSE
)
neest_df <- bind_rows(neest_df, missing_rows)

####
# save
write.csv(neest_df, "results/NeEstimator/summary_Ne.csv")
