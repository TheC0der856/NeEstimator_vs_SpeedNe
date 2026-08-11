# mamba activate msprime
import os
import msprime

Ne = 1000
n_individuals = 100
sequence_length = 100_000_000
mutation_rate = 1e-8
recombination_rate = 1e-8

for i in range(1, 51):

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

    filename = f"Ne1000_{i:02d}.vcf"

    with open(filename, "w") as f:
        mts.write_vcf(f)

    print(f"Simulation {i}/50: {mts.num_sites} SNPs -> {filename}")

