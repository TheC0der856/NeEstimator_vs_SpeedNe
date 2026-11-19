#!/bin/bash
#SBATCH --job-name=N10000
#SBATCH --array=1-30%4
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --time=24:00:00
#SBATCH --output=logs/N10000_%A_%a.out
#SBATCH --error=logs/N10000_%A_%a.err
#SBATCH --mail-type=END

# activate environment
source ~/.bashrc
mamba activate msprime

# go to working directory
cd /scratch/utr_gronefeld/simulate_SNP_datasets


# Run Python simulation
python N1000.py ${SLURM_ARRAY_TASK_ID}



# deactivate environment
mamba deactivate
