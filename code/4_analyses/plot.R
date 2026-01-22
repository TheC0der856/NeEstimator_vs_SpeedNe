library(dplyr)
library(ggplot2)
library(patchwork)

# Load data
errors <- read.csv("results/errors.csv")
time_mem <- read.csv("results/time_memory.csv")

# combine results
results <- errors %>%
  select(Tool, marker, dataset, na_percent) %>%  # nur relevante Spalten
  inner_join(
    time_mem %>% select(Tool, marker, file, seconds, memory_mb),
    by = c("Tool", "marker", "dataset" = "file")  # dataset entspricht file
  )

# rename to plot
results_plot <- results %>%
  mutate(Group = case_when(
    marker == "SNP" ~ "SNP",
    marker == "microsatellite" ~ "Microsatellite",
    TRUE ~ "All"
  ))
results_plot <- bind_rows(
  results_plot,
  results_plot %>% mutate(Group = "All")
)

# Layout
my_theme <- theme_minimal() +
  theme(
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(size = 0.5, color = "black"),
    axis.line = element_line(size = 0.5, color = "black")
  )

# Plots
(
  ggplot(results_plot, aes(x = Group, y = memory_mb, fill = Tool)) +
    geom_boxplot(show.legend = FALSE) +
    labs(title = "Memory [MB]", y = "", x = "") +
    scale_fill_manual(values = c("grey", "grey30")) +
    my_theme
) | (
  ggplot(results_plot, aes(x = Group, y = seconds, fill = Tool)) +
    geom_boxplot(show.legend = FALSE) +
    labs(title = "Time [sec]", y = "", x = "") +
    scale_y_log10() +
    scale_fill_manual(values = c("grey", "grey30")) +
    my_theme
) | (
  ggplot(results_plot, aes(x = Group, y = na_percent, fill = Tool)) +
    geom_boxplot(show.legend = FALSE) +
    labs(title = "Errors [%]", y = "", x = "") +
    scale_fill_manual(values = c("grey", "grey30")) +
    my_theme
)