# load libraries
library(ggplot2)
library(dplyr)
library(tidyr) # pivot_longer
library(ggh4x)

# load results
SpeedNe <- read.csv("results/simulate/SpeedNe.csv")
NeEstimator <- read.csv("results/simulate/RLDNe.csv")

# extract Ne and dataset information
SpeedNe <- SpeedNe %>%
  select(dataset, 
         SpeedNe_Ne = Ne) 
NeEstimator <- NeEstimator %>%
  select(
    dataset = N_folder,
    NeEstimator_Ne = Ne,
  ) 

# unite information
results <- bind_cols(
  SpeedNe,
  NeEstimator %>% select(NeEstimator_Ne)
) %>%
  select(dataset, SpeedNe_Ne, NeEstimator_Ne)
#results

# new data fromat for the graph
# exclude negative Ne
# exclude in C > 2000 
# exclude in D > 20000
# excluded in general: 
# dataset Method      Original Excluded Remaining
# <chr>   <chr>          <int>    <int>     <int>
#   1 N10     NeEstimator       30        0        30
# 2 N10     SpeedNe           30        0        30
# 3 N100    NeEstimator       30        0        30
# 4 N100    SpeedNe           30        0        30
# 5 N1000   NeEstimator       30        5        25
# 6 N1000   SpeedNe           30        0        30
# 7 N10000  NeEstimator       30       15        15
# 8 N10000  SpeedNe           30        0        30
results_long <- results %>%
  pivot_longer(
    cols = c(SpeedNe_Ne, NeEstimator_Ne),
    names_to = "Method",
    values_to = "Ne"
  ) %>%
  filter(Ne >= 0,
         #!(dataset == "N1000" & Ne > 2000),
         #!(dataset == "N10000" & Ne > 20000)
  ) %>%
  mutate(
    Method = recode(
      Method,
      SpeedNe_Ne = "sᴘᴇᴇᴅ-ɴᴇ",
      NeEstimator_Ne =  "NᴇEꜱᴛɪᴍᴀᴛᴏʀ"
    ),
    dataset = factor(
      dataset,
      levels = c("N10", "N100", "N1000", "N10000")
    )
  )

reference_lines <- data.frame(
  dataset = factor(
    c("N10", "N100", "N1000", "N10000"),
    levels = c("N10", "N100", "N1000", "N10000")
  ),
  reference_line = c(10, 100, 1000, 10000)
)

# show results
ggplot(results_long, aes(x = Method, y = Ne, fill = Method)) +
  geom_hline(
    data = reference_lines,
    aes(yintercept = reference_line),
    color = "grey",
    linewidth = 0.5,
    inherit.aes = FALSE
  ) +
  geom_boxplot() +
  facet_wrap(
    ~ dataset, scales = "free_y", ncol = 4,
    labeller = as_labeller(c(
      N10 = "A", N100 = "B", N1000 = "C", N10000 = "D"
    ))
  ) +
  facetted_pos_scales(
    y = list(
      NULL,
      NULL,
      scale_y_continuous(
        limits = c(0, 2000),
        oob = scales::oob_keep
      ),
      scale_y_continuous(
        limits = c(0, 20000),
        oob = scales::oob_keep
      )
    )
  ) +
  labs(
    x = "Program", y = expression(italic(N)[e])
  ) +
  scale_fill_manual(values = c("grey", "grey30")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_line(linewidth = 0.5, color = "black"),
    axis.line = element_line(linewidth = 0.5, color = "black")
  )
  


# Wilcox test
# needs a different filter! all outliers required
results_long <- results %>%
  pivot_longer(
    cols = c(SpeedNe_Ne, NeEstimator_Ne),
    names_to = "Method",
    values_to = "Ne"
  ) %>%
  filter(Ne >= 0) %>%
  mutate(
    Method = recode(
      Method,
      SpeedNe_Ne = "sᴘᴇᴇᴅ-ɴᴇ",
      NeEstimator_Ne =  "NᴇEꜱᴛɪᴍᴀᴛᴏʀ"
    ),
    dataset = factor(
      dataset,
      levels = c("N10", "N100", "N1000", "N10000")
    )
  ) %>%
  group_by(dataset) %>%
  filter(
    Method != lag(Method, default = first(Method)) |
      row_number() == 1
  ) %>%
  ungroup()


results_test <- results_long %>%
  mutate(
    reference_N = as.numeric(gsub("N", "", dataset)),
    absolute_distance = abs(Ne - reference_N)
  )

wilcox.test(
  x = results_test$absolute_distance[results_test$Method == "sᴘᴇᴇᴅ-ɴᴇ"],
  y = results_test$absolute_distance[results_test$Method == "NᴇEꜱᴛɪᴍᴀᴛᴏʀ"],
  paired = TRUE,
  alternative = "two.sided"
)
