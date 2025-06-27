#!/bin/bash
#SBATCH -c 1                              # 1 cores
#SBATCH -t 0-00:30                         # Runtime of 8 hours, in D-HH:MM format
#SBATCH --mem=20G                          # Memory total in 10 GB
#SBATCH -p short                          # Run in a partition: short
#SBATCH -o /n/groups/patel/akshaya/01_ukb/out/batch_proteome_stdout/slurm_bmi_proteome_assoc_%A_%a.out      # File to which STDOUT + STDERR will be written
#SBATCH --mail-type=ALL                   # ALL email notification type
#SBATCH --mail-user=akshayaravi@college.harvard.edu  # Email to which notifications will be sent
#SBATCH --array=2-47


#Load modules
module load gcc/14.2.0
module load R/4.4.2

#Methodology

#give enough temp file space
ulimit -n 10000

# Ensure output directory exists
mkdir -p "/n/groups/patel/akshaya/01_ukb/out/slurm_proteome_assoc/"

#Specify file directories:
#NEVER put spaces in bash!!!
r_path="/n/groups/patel/akshaya/01_ukb/scripts/proteome/batch_proteome_assoc.R"

# Execute the R script 
Rscript "$r_path" ${SLURM_ARRAY_TASK_ID}

echo "Batch Proteome with BMI Association Job Complete"
