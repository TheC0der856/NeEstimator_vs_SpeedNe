import os
import msprime

Ne_current = 20
Ne_bottleneck = 5
n_individuals = 100
sequence_length = 100_000_000
mutation_rate = 1e-8
recombination_rate = 1e-8

# 500 generations ago the Ne was 5, afterwards and before 20
demography = msprime.Demography()
demography.add_population(name="pop", initial_size=Ne_current)
demography.add_population_parameters_change(time=500, initial_size=Ne_bottleneck, population="pop")
demography.add_population_parameters_change(time=1000, initial_size=Ne_current, population="pop")

for i in range(1, 31):

    ts = msprime.sim_ancestry(
        samples={"pop": n_individuals},
        demography=demography,
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

    filename = f"Bottleneck20to5to20_{i:02d}.vcf"

    with open(filename, "w") as f:
        mts.write_vcf(f)

    print(
        f"Simulation {i}/30: "
        f"{mts.num_sites} SNPs -> {filename}"
    )




