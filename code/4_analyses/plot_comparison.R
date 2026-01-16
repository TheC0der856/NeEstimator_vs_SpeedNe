# plot comparison

# error plot
errors <- data.frame(
  category    = c("all", "SNP", "microsatellite"),
  NeEstimator = c(30, 40, 20),
  SpeedNe     = c(77, 100, 53)
)

time <- data.frame(
  category    = c("all", "SNP", "microsatellite"),
  NeEstimator = c(5, 6, 5),
  SpeedNe     = c(2606, 5144, 66)
)

memory <- data.frame(
  category    = c("all", "SNP", "microsatellite"),
  NeEstimator = c(406, 430, 381),
  SpeedNe     = c(1334, 966, 1702)
)
  

# preparations
errors$Metric <- "Errors [%]"
time$Metric <- "Time [sec]"
memory$Metric <- "Memory [MB]"
combined <- rbind(errors, time, memory)

# long format
combined_long <- pivot_longer(combined, cols = c("NeEstimator", "SpeedNe"),
                              names_to = "Tool", values_to = "Value")

ggplot(combined_long, aes(x = category, y = Value, fill = Tool)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +   
  scale_fill_manual(values = c("grey", "black")) +
  labs(x = "", y = "", title = "") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),         # Achsentexte
    axis.ticks = element_line(size = 0.5, color = "black"),  # Ticks sichtbar
    axis.line = element_line(size = 0.5, color = "black"),   # Achsenlinien sichtbar
    strip.text = element_text(size = 15),        # Facet-Titel
    panel.grid = element_blank()                 # Keine Gitterlinien
  )
