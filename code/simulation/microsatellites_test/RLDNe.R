# load libraries
library(RLDNe)
library(tidyr)
library(dplyr)


### load data
csv <- read.csv("data/simulate_data/Ne_size/Ne10000/Ne10000_01.csv")
#head(csv[,1:10])

### format gen_df like the example file:
data("wgp_example_2col")
head(wgp_example_2col[,1:10])

# create ind_id and pop
colnames(csv)[colnames(csv) == "Individual"] <- "ind_id"
csv <- data.frame(pop = "pop1", csv)
# organise loci
csv <- csv %>%
  separate_wider_delim(
    cols = starts_with("Locus_"),
    delim = "/",
    names = c("1", "2"),
    names_sep = "_"
  )
#head(csv[,1:10])


# run NeEstimator:
efgl <- readInData(csv ,genotypeStart = 3,pedigreeColumn = 1,nameColumn = 2, missingAlleles ="NA")
rldne <- exportGenePop_RLDNe(EFGLdata = efgl)
rldne <- create_LDNe_params(rldne)
std_out <- run_LDNe(rldne)
df <- read_LDNeOutFile(rldne)
df