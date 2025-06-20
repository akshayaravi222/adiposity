#!/bin/bash
#SBATCH -c 1                              # 1 cores
#SBATCH -o=logs/chr22_step1_%j.out
#SBATCH --error=logs/chr22_step1_%j.err
#SBATCH -p short                          # Run in a partition: short
#SBATCH --time=0-02:00
#SBATCH --mem=32G
#SBATCH --cpus-per-task=4
#SBATCH --mail-type=ALL                   # ALL email notification type
#SBATCH --mail-user=akshayaravi@college.harvard.edu  # Email to which notifications will be sent

conda activate regenie_env

regenie \
  --step 1 \
  --bed ukb_chr22_init_filtere \
  --phenoFile ukb_pheno.txt \
  --phenoCol android_bone_mass_f23244_2_0 \
  --covarFile ukb_covar.txt \
  --covarCol age,sex,pc1,pc2,pc3,pc4,pc5,pc6,pc7,pc8,pc9,pc10,age2,age_sex,bmi \
  --bsize 1000 \
  --threads 4 \
  --out chr_22_step1_output


