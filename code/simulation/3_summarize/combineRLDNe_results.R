library(dplyr)

# summarize RLDNe results

# part of all
constant_n100_Ne10 <- read.csv("results/simulate/Ne_size/RLDNe/Ne10.csv")
# rename Pop
constant_n100_Ne10$Pop <- paste0("constant_n100_Ne10_",sprintf("%02d", 1:30))

# demography
bottleneck <- read.csv("results/simulate/demography/RLDNe/bottleneck.csv")
decline <- read.csv("results/simulate/demography/RLDNe/decline.csv")
expansion <- read.csv("results/simulate/demography/RLDNe/expansion.csv")

# Ne size
Ne50 <- read.csv("results/simulate/Ne_size/RLDNe/Ne50.csv")
Ne100 <- read.csv("results/simulate/Ne_size/RLDNe/Ne100.csv")

# sample size
n50_Ne5 <- read.csv("results/simulate/sample_size/RLDNe/n50_Ne5.csv")
n500_Ne50 <- read.csv("results/simulate/sample_size/RLDNe/n500_Ne50.csv")



# add to one dataset:
all_results <- bind_rows(
  constant_n100_Ne10,
  bottleneck,
  decline,
  expansion,
  Ne50,
  Ne100,
  n50_Ne5,
  n500_Ne50
)
write.csv(all_results, "results/simulate/summary/RLDNe_all.csv")
