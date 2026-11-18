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


############ show results:
results$dataset <- factor(
  results$dataset,
  levels = c("constant", "decline", "expansion", "bottleneck")
)

ggplot(results, aes(x = dataset, y = absolute_error, fill = Method)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(
    x = "",
    y = expression("|" * italic("Simulated ") * N[e] *
                     " - " * italic("Estimated ") * N[e] * "|"),
    fill = ""
  ) +
  scale_fill_manual(values = c("grey", "grey30")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    #legend.text = element_text(size = 12),
    panel.grid = element_blank(),
    axis.ticks = element_line(size = 0.5, color = "black"),
    axis.line = element_line(size = 0.5, color = "black")
  )



##### Are the differences in results significant?
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



