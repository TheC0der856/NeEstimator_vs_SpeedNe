library(ggplot2)
library(dplyr)

SpeedNe <- read.csv("results/simulate/summary/SpeedNe.csv")
NeEstimator <- read.csv("results/simulate/summary/RLDNe_all.csv")

# True Ne für jedes Szenario
true_values <- data.frame(
  dataset = c("constant", "expansion", "decline", "bottleneck"),
  true_Ne = c(10, 20, 10, 20)
)

# SpeedNe, CI is also jackknife over loci
SpeedNe_results <- SpeedNe %>%
  select(dataset, lower_CI, upper_CI) %>%
  rename(
    low_jack = lower_CI,
    high_jack = upper_CI
  ) %>%
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
  select(dataset, low_jack, high_jack, Method)

# Beide Programme zusammenführen + True Ne hinzufügen
results <- bind_rows(
  SpeedNe_results,
  NeEstimator_results
) %>%
  left_join(true_values, by = "dataset") 


# Analyse coverage and width of intervals
results_CI <- results %>%
  mutate(
    covered = low_jack <= true_Ne & high_jack >= true_Ne
  )
coevrage <- results_CI %>%
  group_by(dataset, Method) %>%
  summarise(
    coverage = mean(covered),
    n = n(),
    .groups = "drop"
  )

coverage_comparison <- coevrage %>%
  select(dataset, Method, coverage) %>%
  tidyr::pivot_wider(
    names_from = Method,
    values_from = coverage
  ) %>%
  mutate(
    NeEstimator = NeEstimator * 100,
    SpeedNe = SpeedNe * 100,
    difference = NeEstimator - SpeedNe
  )

coverage_comparison


results_CI <- results_CI %>%
  mutate(CI_width = high_jack - low_jack)

############ show differences in confidence interval width:
results_CI$dataset <- factor(
  results_CI$dataset,
  levels = c("constant", "decline", "expansion", "bottleneck")
)
ggplot(results_CI, aes( x = dataset,  y = CI_width,  fill = Method)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(x = "", y = "Confidence interval width"  ) +
  scale_fill_manual(values = c("grey80", "grey30")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(linewidth = 0.5, color = "black"),
    axis.line = element_line(linewidth = 0.5, color = "black")
  )

### are the confidence intervals significantely smaller?
comparison <- results_CI %>%
  group_by(dataset, Method) %>%
  mutate(simulation = row_number()) %>%
  ungroup() %>%
  select(dataset, Method, simulation, CI_width) %>%
  tidyr::pivot_wider(
    names_from = Method,
    values_from = CI_width
  )
# Paired Wilcoxon test
wilcox.test(
  comparison$SpeedNe,
  comparison$NeEstimator,
  paired = TRUE
)
# yes! significant! ***

# Are the CI of NeEstimator containing Ne more often significantly?
mcnemar_results <- coverage_test %>%
  group_by(dataset) %>%
  group_modify(~ {
    tab <- table(SpeedNe = .x$SpeedNe,NeEstimator = .x$NeEstimator)
    test <- mcnemar.test(tab)
    tibble(
      statistic   = unname(test$statistic),
      p_value     = test$p.value
    )
  }) %>%
  ungroup()

mcnemar_results

# no! not significant!
