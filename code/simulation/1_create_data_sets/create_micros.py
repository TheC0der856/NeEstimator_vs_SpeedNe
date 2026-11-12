import msprime       # needed to simulate dataset
import pandas as pd  # needed to create a result table
import sys           # communication with bash


# definitions
run           = int(sys.argv[1]) # job number
popsize       = int(sys.argv[2]) # population size
n_individuals = 50               # number of individuals
n_locus       = 50               # number of independent loci
data          = {"Individual": [f"Ind_{i+1}" for i in range(n_individuals)]}

# define model parameters
ts = msprime.sim_ancestry(
    samples           =[msprime.SampleSet(num_samples=n_individuals, ploidy=2)],
    population_size   =popsize,	       # effective population size = population size, only with using a standard coalescens model and no complex demographic history
    sequence_length   =n_locus,        # number of positions, in my case loci: we have to analyse them together so LD can be found
    recombination_rate=1e-8,           # recombination rate of humans, idk?
    random_seed       =1+run+popsize   # reproducability
)

# simulate mutations
mts = msprime.sim_mutations(
    ts,                                                         # enter model parameters
    rate       =1e-4,                                           # normal mutation rate for microsatellites: "Every microsatellite is different: Intrinsic DNA features dictate mutagenesis of common microsatellites present in the human genome"
    model      =msprime.SMM(                                    # Stepwise Mutation Model (SMM)
	lo=2, hi=50),                                           # lowest and highest possible number of repeats
    random_seed=100+run+popsize                                 # reproducability
)

# find genotype per locus
samples = mts.samples()
variants_by_position = {int(v.site.position): v for v in mts.variants()}
for locus_idx in range(n_locus):
    variant = variants_by_position.get(locus_idx)
    alleles = []
    for individual in mts.individuals():
        nodes = individual.nodes
        if variant is None:
            alleles.append("NA/NA")
        else:
            genotype_by_node = dict(zip(samples, variant.genotypes))
            a1, a2 = [variant.alleles[genotype_by_node[n]] for n in nodes]
            alleles.append(f"{a1}/{a2}")

    data[f"Locus_{locus_idx+1}"] = alleles

# create and save result table
df       = pd.DataFrame(data)
filename = f"Ne{popsize}_{run:02d}.csv"
filepath = f"Ne{popsize}/{filename}"
df.to_csv(filepath, index=False)
