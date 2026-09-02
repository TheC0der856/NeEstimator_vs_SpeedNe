library(ggplot2)
library(dplyr)

SpeedNe <- read.csv("results/simulate/summary/SpeedNe.csv")
NeEstimator <- read.csv("results/simulate/summary/RLDNe_all.csv")

library(dplyr)

# True Ne für jedes Szenario
true_values <- data.frame(
  dataset = c("constant", "expansion", "decline", "bottleneck"),
  true_Ne = c(10, 20, 10, 20)
)

# SpeedNe
SpeedNe_results <- SpeedNe %>%
  select(dataset, Ne) %>%
  mutate(Method = "SpeedNe")

# NeEstimator
NeEstimator_results <- NeEstimator %>%
  mutate(
    dataset = case_when(
      grepl("constant_n100_Ne10", Pop) ~ "constant",
      grepl("expansion10to20", Pop) ~ "expansion",
      grepl("decline20to10", Pop) ~ "decline",
      grepl("Bottleneck20to5to20", Pop) ~ "bottleneck",
      TRUE ~ NA_character_
    ),
    Method = "NeEstimator"
  ) %>%
  filter(!is.na(dataset)) %>%
  select(dataset, Ne, Method)

# Beide Programme zusammenführen + True Ne hinzufügen
results <- bind_rows(
  SpeedNe_results,
  NeEstimator_results
) %>%
  left_join(true_values, by = "dataset") %>%
  mutate(
    error = Ne - true_Ne,
    absolute_error = abs(error),
    relative_error = absolute_error / true_Ne
  )

# Accuracy vergleichen
accuracy <- results %>%
  group_by(dataset, Method) %>%
  summarise(
    Bias = mean(error, na.rm = TRUE),
    MAE = mean(absolute_error, na.rm = TRUE),
    RMSE = sqrt(mean(error^2, na.rm = TRUE)),
    Relative_Error = mean(relative_error, na.rm = TRUE),
    .groups = "drop"
  )

accuracy




# Absolute Fehler pro Simulation
comparison <- results %>%
  group_by(dataset, Method) %>%
  mutate(simulation = row_number()) %>%
  ungroup() %>%
  select(dataset, Method, simulation, absolute_error) %>%
  tidyr::pivot_wider(
    names_from = Method,
    values_from = absolute_error
  )

# Paired Wilcoxon test
wilcox.test(
  comparison$SpeedNe,
  comparison$NeEstimator,
  paired = TRUE
)