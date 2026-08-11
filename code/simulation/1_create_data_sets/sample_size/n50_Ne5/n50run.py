import os
import msprime

Ne = 5
n_individuals = 50
sequence_length = 100_000_000
mutation_rate = 1e-8
recombination_rate = 1e-8

for i in range(1, 31):

    ts = msprime.sim_ancestry(
        samples=n_individuals,
        population_size=Ne,
        sequence_length=sequence_length,
        recombination_rate=recombination_rate,
        ploidy=2,
        random_seed=42 + i
    )

    mts = msprime.sim_mutations(
        ts,
        rate=mutation_rate,
        random_seed=1000 + i
    )

    filename = f"n50_{i:02d}.vcf"

    with open(filename, "w") as f:
        mts.write_vcf(f)

    print(f"Simulation {i}/30: {mts.num_sites} SNPs -> {filename}")


