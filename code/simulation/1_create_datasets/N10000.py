# import packages
import os
import msprime
import numpy as np #random selection of SNPs
import math
import sys

# simulation number
i = int(sys.argv[1])

# definitions
N                  = 10000           # N is the population size, with standard coalescens model and no complex demographic history it should equal the effective population size
n_individuals      = 50              # number of individuals in the dataset
chromosome_length  = 100_000_000
n_chromosomes      = 10
mutation_rate      = 4e-9
recombination_rate = 2e-8
max_SNPs           = 300

# Create chromosome intervals
positions = [0]
rates = []
chromosome_intervals = []
for j in range(n_chromosomes):
    start = positions[-1]
    end = start + chromosome_length
    chromosome_intervals.append((start, end))
    positions.append(end)
    rates.append(recombination_rate)
    if j < n_chromosomes - 1:
        positions.append(end + 1)
        rates.append(math.log(2))
rate_map = msprime.RateMap(
    position=positions,
    rate=rates
)

# Simulate ancestry
ts = msprime.sim_ancestry(
    samples           =n_individuals,
    population_size   =N,
    recombination_rate=rate_map,
    random_seed       =42+i
)
# Simulate mutations
mts = msprime.sim_mutations(
    ts,
    rate=mutation_rate,
    random_seed=123+i
)

# Identify SNPs
all_sites = np.array([
    variant.site.id
    for variant in mts.variants()
    if len(set(variant.genotypes)) > 1
])

# Random selection of SNPs
if len(all_sites) > max_SNPs:
    rng = np.random.default_rng(42+i)
    selected_sites = np.sort(
        rng.choice(
            all_sites,
            size=max_SNPs,
            replace=False
        )
    )
else:
    selected_sites = all_sites
site_mask = np.ones(mts.num_sites, dtype=bool)
site_mask[selected_sites] = False

# Export VCF
path = os.path.join(r"try",f"N{N}")
os.makedirs(path, exist_ok=True)
filename = f"N{N}_{i:02d}.vcf"

with open(os.path.join(path, filename), "w") as f:
     mts.write_vcf(
     f,
     site_mask=site_mask,
     allow_position_zero=True
)