library(RLDNe)
library(vcfR)
library(adegenet)

### load data
vcf <- read.vcfR("data/simulate_data/Ne_size/Ne100/Ne1000_01.vcf")
gen <- vcfR2genind(vcf)
gen_df <- genind2df(gen)
#head(gen_df[,1:10])


### format gen_df like the example file:
#data("wgp_example_2col")
#head(wgp_example_2col[,1:10])

# create ind_id and pop
gen_df_2col <- data.frame(pop = 1, ind_id = rownames(gen_df),  check.names = FALSE)
# include genotypes
for (i in seq_along(gen_df)) {
  x <- gen_df[[i]]
  gen_df_2col[[paste0("Locus_", i, "_1")]] <- substr(x, 1, 1)
  gen_df_2col[[paste0("Locus_", i, "_2")]] <- substr(x, 2, 2)
}
# instead of 0 and 1, we need 1 and 2
gen_df_2col[, 3:ncol(gen_df_2col)] <- lapply(gen_df_2col[, 3:ncol(gen_df_2col)],as.numeric)
gen_df_2col[, 3:ncol(gen_df_2col)] <-  gen_df_2col[, 3:ncol(gen_df_2col)] + 1
#head(gen_df_2col[,1:10])

# run NeEstimator:
efgl <- readInData(gen_df_2col ,genotypeStart = 3,pedigreeColumn = 1,nameColumn = 2)
rldne <- exportGenePop_RLDNe(EFGLdata = efgl)
rldne <- create_LDNe_params(rldne)
std_out <- run_LDNe(rldne)
df <- read_LDNeOutFile(rldne)

df
