# to use this code, you have to run the code in Ne_size folder first

n100_Ne10 <- read.csv("results/simulate/Ne_size/RLDNe/Ne10.csv")

# hier it would be possible to additionally save the file with another popname like n100_Ne10_*

write.csv(n100_Ne10 , "results/simulate/sample_size/RLDNe/n100_Ne10.csv", row.names = FALSE)