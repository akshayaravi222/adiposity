#!/bin/bash
#SBATCH -c 1                              # 1 cores
#SBATCH -t 0-00:20                         # Runtime of 8 hours, in D-HH:MM format
#SBATCH --mem=30G                          # Memory total in 10 GB
#SBATCH -p short                          # Run in a partition: short
#SBATCH -o /n/groups/patel/akshaya/01_ukb/out/batch_imp_expo_stdout/slurm_imp_expo_assoc_%A_%a.out      # File to which STDOUT + STDERR will be written
#SBATCH --mail-type=ALL                   # ALL email notification type
#SBATCH --mail-user=akshayaravi@college.harvard.edu  # Email to which notifications will be sent
#SBATCH --array=2-33


#Load modules
module load gcc/9.2.0
module load R/4.2.1
module load cmake/3.22.2

#Methodology

#give enough temp file space
ulimit -n 10000

# Ensure output directory exists
mkdir -p "/n/groups/patel/akshaya/01_ukb/out/slurm_imp_expo_assoc/"

#Specify file directories:
#NEVER put spaces in bash!!!
r_path="/n/groups/patel/akshaya/01_ukb/scripts/impedance/batch_impedance_assoc.R"

# Execute the R script 
Rscript "$r_path" ${SLURM_ARRAY_TASK_ID}

echo "Batch Impedance-Protein Association Task Complete"

