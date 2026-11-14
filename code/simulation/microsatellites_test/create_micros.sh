#!/bin/bash
#SBATCH --job-name=Sim_micro
#SBATCH --array=0-119%10
#SBATCH --cpus-per-task=1
#SBATCH --mem=2G
#SBATCH --time=01:00:00
#SBATCH --output=logs/microsat_%A_%a.out
#SBATCH --error=logs/microsat_%A_%a.err
#SBATCH --mail-type=END

# activate environment
source ~/.bashrc
mamba activate msprime

# go to working directory
cd /scratch/utr_gronefeld/simulate_SNP_datasets

# Population sizes
POPSIZES=(10 100 1000 10000)

# Determine simulation number
POP_IDX=$((SLURM_ARRAY_TASK_ID / 30))
RUN=$((SLURM_ARRAY_TASK_ID % 30 + 1))

# Get population size
POPSIZE=${POPSIZES[$POP_IDX]}

# Create output directory
mkdir -p "Ne${POPSIZE}"

# Run Python simulation
python create_microsat.py $RUN $POPSIZE



# deactivate environment
mamba deactivate
