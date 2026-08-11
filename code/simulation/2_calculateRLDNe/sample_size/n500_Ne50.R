# load libraries
library(RLDNe)
library(vcfR)
library(adegenet)

# prepare:
vcf_dir <- "data/simulate_data/sample_size/n500_Ne50"
df_results <- data.frame()

# calculate and save relevant results:
for (i in 1:30) {
  pop_name <- sprintf("n500_%02d", i)
  vcf_path <- file.path(vcf_dir, paste0(pop_name, ".vcf"))
  
  # load data and transform to df
  vcf <- read.vcfR(vcf_path)
  gen <- vcfR2genind(vcf)
  gen_df <- genind2df(gen)
  
  ### format gen_df like the example file:
  #data("wgp_example_2col")
  #head(wgp_example_2col[,1:10])
  # pop & ind names
  gen_df_2col <- data.frame(pop = 1, ind_id = rownames(gen_df), check.names = FALSE  ) 
  # add genotypes
  for (j in seq_along(gen_df)) {
    x <- gen_df[[j]]
    gen_df_2col[[paste0("Locus_", j, "*1")]] <- substr(x, 1, 1)
    gen_df_2col[[paste0("Locus_", j, "*2")]] <- substr(x, 2, 2)
  } 
  # 0/1 -> 1/2
  gen_df_2col[, 3:ncol(gen_df_2col)] <-  lapply(gen_df_2col[, 3:ncol(gen_df_2col)], as.numeric)
  gen_df_2col[, 3:ncol(gen_df_2col)] <-   gen_df_2col[, 3:ncol(gen_df_2col)] + 1
  
  # NeEstimator
  efgl <- readInData( gen_df_2col, genotypeStart = 3, pedigreeColumn = 1, nameColumn = 2  )
  rldne <- exportGenePop_RLDNe(EFGLdata = efgl)
  rldne <- create_LDNe_params(rldne)
  std_out <- run_LDNe(rldne)
  df <- read_LDNeOutFile(rldne)
  
  # CritValue = 0.00, better to evaluate precision?
  df_selected  <- df[4, , drop = FALSE]
  df_selected$Pop <- pop_name
  
  # create final df
  df_results <- rbind(df_results, df_selected)
}

df_results
write.csv(df_results, "results/simulate/sample_size/RLDNe/n500_Ne50.csv", row.names = FALSE)