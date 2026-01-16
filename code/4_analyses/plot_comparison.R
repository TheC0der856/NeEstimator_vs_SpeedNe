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
  
barplot(
  t(as.matrix(errors[, c("NeEstimator", "SpeedNe")])), # transponieren, damit Kategorien auf x-Achse
  beside = TRUE,                                       # Bars nebeneinander
  names.arg = errors$category,                         # Kategorien auf x-Achse
  col = c("grey", "black"),                        # Farben für die Tools
  #legend.text = c("NeEstimator", "SpeedNe"),          # Legende
  args.legend = list(x = "topright"),
  ylab = "Errors [%]"
)

barplot(
  t(as.matrix(time[, c("NeEstimator", "SpeedNe")])), # transponieren, damit Kategorien auf x-Achse
  beside = TRUE,                                       # Bars nebeneinander
  names.arg = time$category,                         # Kategorien auf x-Achse
  col = c("grey", "black"),                        # Farben für die Tools
  #legend.text = c("NeEstimator", "SpeedNe"),          # Legende
  args.legend = list(x = "topright"),
  ylab = "Time [sec]"
)

barplot(
  t(as.matrix(memory[, c("NeEstimator", "SpeedNe")])), # transponieren, damit Kategorien auf x-Achse
  beside = TRUE,                                       # Bars nebeneinander
  names.arg = memory$category,                         # Kategorien auf x-Achse
  col = c("grey", "black"),                        # Farben für die Tools
  #legend.text = c("NeEstimator", "SpeedNe"),          # Legende
  args.legend = list(x = "topright"),
  ylab = "Memory [MB]"
)