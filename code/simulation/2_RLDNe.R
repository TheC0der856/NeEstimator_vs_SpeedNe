# load libraries
library(RLDNe)
library(vcfR)
library(adegenet)

# prepare
base_dir <- "data/simulate_data"
n_folders <- list.dirs(base_dir, recursive = FALSE, full.names = FALSE)
df_results <- data.frame()

# calculate and save relevant results
for (n_folder in n_folders) {
  vcf_dir <- file.path(base_dir, n_folder)
  
  for (i in 1:30) {
    pop_name <- sprintf("%s_%02d", n_folder, i)
    vcf_path <- file.path(vcf_dir, paste0(pop_name, ".vcf"))
    
    # skip if file does not exist
    if (!file.exists(vcf_path)) {
      warning(paste("File not found:", vcf_path))
      next
    }
    
    # load data and transform to df
    vcf <- read.vcfR(vcf_path)
    gen <- vcfR2genind(vcf)
    gen_df <- genind2df(gen)
    
    ### format gen_df like the example file:
    #data("wgp_example_2col")
    #head(wgp_example_2col[,1:10])
    gen_df_2col <- data.frame(
      pop         = 1,
      ind_id      = rownames(gen_df),
      check.names = FALSE
    )
    for (j in seq_along(gen_df)) {
      x <- gen_df[[j]]
      gen_df_2col[[paste0("Locus_", j, "*1")]] <- substr(x, 1, 1)
      gen_df_2col[[paste0("Locus_", j, "*2")]] <- substr(x, 2, 2)
    }
    # 0/1 -> 1/2
    gen_df_2col[, 3:ncol(gen_df_2col)] <- lapply(gen_df_2col[, 3:ncol(gen_df_2col)], as.numeric)
    gen_df_2col[, 3:ncol(gen_df_2col)] <- gen_df_2col[, 3:ncol(gen_df_2col)] + 1
    
    # NeEstimator
    efgl    <- readInData(gen_df_2col, genotypeStart = 3, pedigreeColumn = 1, nameColumn = 2)
    rldne   <- exportGenePop_RLDNe(EFGLdata = efgl)
    rldne   <- create_LDNe_params(rldne)
    std_out <- run_LDNe(rldne)
    df      <- read_LDNeOutFile(rldne)
    
    # select and add information
    df_selected          <- df[2, , drop = FALSE] # CritValue 4 = 0.00, 2 = 0.05
    df_selected$Pop      <- pop_name
    df_selected$N_folder <- n_folder
    
    # append results
    df_results <- rbind(df_results, df_selected)
  }
}

# save results
write.csv(df_results, "results/simulate/RLDNe.csv", row.names = FALSE)