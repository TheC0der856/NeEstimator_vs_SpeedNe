# import packages
import os
import msprime
import numpy as np #random selection of SNPs

# definitions
Ns                 = [10, 100, 1000, 10000] # N is the population size, with standard coalescens model and no complex demographic history it should equal the effective population size
n_individuals      = 50                     # number of individuals in the dataset
sequence_length    = 100_000_000
mutation_rate      = 1e-8                   # mutation and recombination rate inspired by humans
recombination_rate = 1e-8
max_SNPs           = 300

# create 30 vcf files for each population size
for N in Ns:
    for i in range(1, 31): 
        # msprime simulation
        ts = msprime.sim_ancestry(
            samples           =n_individuals,
            population_size   =N,
            sequence_length   =sequence_length,
            recombination_rate=recombination_rate,
            ploidy            =2,                   # diploid
            random_seed       =42 + i               # for reproducability
        )
        mts = msprime.sim_mutations(
            ts,
            rate=mutation_rate,
            random_seed=1000 + i                    # for reproducability
        )
    
        # don't save more than the maximum number of SNPs
        if mts.num_sites > max_SNPs:
            rng = np.random.default_rng(N + i)       
            selected_sites = rng.choice(
                mts.num_sites,
                size   =max_SNPs,
                replace=False
            )       
            sites_to_delete = np.setdiff1d(
                np.arange(mts.num_sites),
                selected_sites
            )        
            mts_subset = mts.delete_sites(sites_to_delete)
        else:
            mts_subset = mts
        
        # save dataset
        path = os.path.join(
            r"C:\Users\Gronefeld\Desktop\Ne_programs\calculations\data\simulate_data",
            f"N{N}"
        )
        os.makedirs(path, exist_ok=True) # create output directory if it does not exist yet
        
        filename = f"N{N}_{i:02d}.vcf"
        
        with open(os.path.join(path, filename), "w") as f:
            mts_subset.write_vcf(f)
            




