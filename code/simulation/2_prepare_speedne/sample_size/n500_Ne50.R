library(vcfR)
library(adegenet)

# prepare:
vcf_dir <- "data/simulate_data/sample_size/n500_Ne50"
df_results <- data.frame()

for (i in 1:30) {
  pop_name <- sprintf("n500_%02d", i)
  vcf_path <- file.path(vcf_dir, paste0(pop_name, ".vcf"))
  
  # load data and transform to df
  vcf <- read.vcfR(vcf_path)
  gen <- vcfR2genind(vcf)
  gen_df <- genind2df(gen)
  
  # 0 and lower will be missing data
  gen_df <- apply(gen_df, c(1, 2), function(x) {
    paste0(as.integer(strsplit(as.character(x), "")[[1]]) + 1, collapse = "")
  })
  gen_df <- matrix(
    as.numeric(gen_df),
    nrow = nrow(gen_df),
    dimnames = dimnames(gen_df)
  )
  
  # every allele has one row
  gen_df <- do.call(cbind, lapply(seq_len(ncol(gen_df)), function(j) {
    t(sapply(gen_df[, j], function(x) as.integer(strsplit(as.character(x), "")[[1]])))
  }))
  
  
  write.table(
    gen_df,
    sprintf("data/simulated_data_into_csv/sample_size/n500_Ne50/n500_%02d.csv", i),
    row.names = FALSE,
    col.names = FALSE,
    sep = ",",
    quote = FALSE
  )
  
}