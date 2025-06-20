#!/bin/bash
#SBATCH -c 1                              # 1 cores
#SBATCH -o=logs/ukb22tobed_chr1_%j.out
#SBATCH --error=logs/ukb22tobed_chr1_%j.err
#SBATCH -p short                          # Run in a partition: short
#SBATCH --time=0-00:30
#SBATCH --mem=64G
#SBATCH --cpus-per-task=4
#SBATCH --mail-type=ALL                   # ALL email notification type
#SBATCH --mail-user=akshayaravi@college.harvard.edu  # Email to which notifications will be sent


# Load PLINK2 (adjust based on your module system)
module load plink2

# Run PLINK2 to convert BGEN → BED with QC filters
plink2 \
  --bgen ukb_imp_chr22_v3.bgen ref-first \
  --sample ukb52887_imp_chr22_v3_s487296.sample \
  --maf 0.01 \
  --geno 0.02 \
  --hwe 1e-6 \
  --make-bed \
  --threads 4 \
  --out ukb_chr22_init_filtered

