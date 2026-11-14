library(vcfR)
library(adegenet)

# prepare:
base_vcf_dir <- "data/simulate_data/"
base_csv_dir <- "data/simulate_data/csv/"

# only search for vcf file, if the folder starts with "N"
vcf_dirs <- list.dirs(base_vcf_dir,recursive = FALSE, full.names = TRUE)
vcf_dirs <- vcf_dirs[grepl("^N", basename(vcf_dirs))]

for (vcf_dir in vcf_dirs) {
  folder_name <- basename(vcf_dir)
  
  csv_dir <- file.path(base_csv_dir, folder_name)
  
  if (!dir.exists(csv_dir)) {dir.create(csv_dir, recursive = TRUE)}
  
  # find files
  vcf_files <- list.files(
    vcf_dir,
    pattern = "\\.vcf$",
    full.names = TRUE
  )
  
  for (i in 1:30) {
    pop_name <- sprintf("%s_%02d", folder_name, i)
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
    
    # save file
    write.table(
      gen_df,
      file.path(csv_dir, paste0(pop_name, ".csv")),
      row.names = FALSE,
      col.names = FALSE,
      sep = ",",
      quote = FALSE
    )
    
  }
}