library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
errors <- read.csv("results/errors.csv")
time_mem <- read.csv("results/time_memory.csv")
# delete rows
time_mem$X <- NULL
errors$mean_negative_percent <- NULL
errors$sd_negative_percent <- NULL
# combine results
results <- errors %>%
  inner_join(time_mem, by = c("program", "marker"))

# prepare data
results_long <- results %>%
  # Wähle die Spalten für Value und SD
  select(program, marker, 
         mean_na_percent, sd_na_percent,
         time_mean_sec, time_sd_sec,
         mem_mean_mb, mem_sd_mb) %>%
  # Pivot für Value und SD gleichzeitig
  pivot_longer(
    cols = c(mean_na_percent, time_mean_sec, mem_mean_mb),
    names_to = "Metric", values_to = "Value"
  ) %>%
  # SD-Spalte passend zur Metric erstellen
  mutate(SD = case_when(
    Metric == "mean_na_percent" ~ sd_na_percent,
    Metric == "time_mean_sec"   ~ time_sd_sec,
    Metric == "mem_mean_mb"     ~ mem_sd_mb
  )) %>%
  # Lesbare Metric-Namen
  mutate(Metric = case_when(
    Metric == "mean_na_percent" ~ "Errors [%]",
    Metric == "time_mean_sec"   ~ "Time [sec]",
    Metric == "mem_mean_mb"     ~ "Memory Utilized [MB]"
  )) %>%
  # Spalten umbenennen
  rename(Tool = program,
         Category = marker) %>%
  # unnötige SD-Spalten entfernen
  select(Tool, Category, Metric, Value, SD)

results_long

# plot
ggplot(results_long, aes(x = Category, y = Value, fill = Tool)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(width = 0.9),
                width = 0.2, color = "black") +
  facet_wrap(~Metric, scales = "free_y") +   
  scale_fill_manual(values = c("grey", "grey30")) +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),                   # Achsentexte
    axis.ticks = element_line(size = 0.5, color = "black"),# Ticks sichtbar
    axis.line = element_line(size = 0.5, color = "black"), # Achsenlinien sichtbar
    strip.text = element_text(size = 15),                  # Facet-Titel
    panel.grid = element_blank()                           # Keine Gitterlinien
  )