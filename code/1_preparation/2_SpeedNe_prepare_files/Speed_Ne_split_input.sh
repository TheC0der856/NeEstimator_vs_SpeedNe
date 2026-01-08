#!/bin/bash
#SBATCH --job-name=SpeedNe          # job name
#SBATCH --output=Ne_%j.out          # name output
#SBATCH --error=Ne_%j.err           # name error
#SBATCH --time=00:20:00             # time limit hh:mm:ss
#SBATCH --nodes=1                   # one node
#SBATCH --ntasks=1                  # one task
#SBATCH --cpus-per-task=1           # 1 CPU
#SBATCH --mem=1G                    # 1 GB memory reserved
#SBATCH --partition=skylake-96      # partition
#SBATCH --mail-type=END             # message me about the timing and RAM

# path
cd  /scratch/utr_gronefeld/Ne/Ne_programs

# activate environment
source ~/.bashrc
mamba activate RLDNe_env

# run calculation
Rscript code/Speed_Ne_split_input.sh

# deactivate environment
mamba deactivate
