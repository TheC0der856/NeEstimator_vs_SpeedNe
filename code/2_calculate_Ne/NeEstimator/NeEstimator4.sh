#!/bin/bash
#SBATCH --job-name=NeEstimator      # job name
#SBATCH --output=RLDNe_%A_%a.out    # name output
#SBATCH --error=RLDNe_%A_%a.err     # name error
#SBATCH --time=00:20:00             # time limit hh:mm:ss
#SBATCH --nodes=1                   # one node
#SBATCH --ntasks=1                  # one task
#SBATCH --cpus-per-task=1           # 1 CPU
#SBATCH --mem=1G                    # 1 GB memory reserved
#SBATCH --array=4-4                 # job array: 30 jobs
#SBATCH --partition=skylake-96
#SBATCH --mail-type=END             # message me about the timing and RAM

# path
cd  /scratch/utr_gronefeld/Ne/Ne_programs

# activate environment
source ~/.bashrc
mamba activate RLDNe_env

# find files that should be used for the calculation
FILES=(datasets/*.xlsx) # find files
FILE=${FILES[$SLURM_ARRAY_TASK_ID-1]}  # start array with 1

# run calculation
Rscript code/NeEstimator4.R "$FILE"

# deactivate environment
mamba deactivate