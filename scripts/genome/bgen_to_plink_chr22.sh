#!/bin/bash
#SBATCH -c 1                              # 1 cores
#SBATCH -o=/n/groups/patel/akshaya/scripts/logs/ukb22tobed_chr22.out
#SBATCH --error=/n/groups/patel/akshaya/scripts/logs/ukb22tobed_chr22.err
#SBATCH -p short                          # Run in a partition: short
#SBATCH --time=0-00:30
#SBATCH --mem=32G
#SBATCH --cpus-per-task=4
#SBATCH --mail-type=ALL                   # ALL email notification type
#SBATCH --mail-user=akshayaravi@college.harvard.edu  # Email to which notifications will be sent


# Load PLINK2 (adjust based on your module system)
# module load plink

# Run PLINK2 to convert BGEN → BED with QC filters
./plink2 \
  --bgen /n/no_backup2/patel/uk_biobank/ukb_genetics/52887/project_52887_genetics/ukb_imp_chr22_v3.bgen ref-first \
  --sample /n/no_backup2/patel/uk_biobank/ukb_genetics/52887/project_52887_genetics/ukb52887_imp_chr22_v3_s487296.sample \
  --maf 0.01 \
  --geno 0.02 \
  --hwe 1e-6 \
  --make-bed \
  --threads 4 \
  --out ./ukb_chr22

