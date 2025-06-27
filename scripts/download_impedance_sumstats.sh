#!/bin/bash
#SBATCH -c 1                              # 1 cores
#SBATCH -t 0-00:30                         # Runtime of 8 hours, in D-HH:MM format
#SBATCH --mem=5G                          # Memory total in 10 GB
#SBATCH -p short                          # Run in a partition: short
#SBATCH -o /n/groups/patel/akshaya/01_ukb/out/sumstat_download_stdout/sumstat_%A_%a.out      # File to which STDOUT + STDERR will be written
#SBATCH --mail-type=ALL                   # ALL email notification type
#SBATCH --mail-user=akshayaravi@college.harvard.edu  # Email to which notifications will be sent
#SBATCH --array=1-4

# Load field from the N-th line
FIELD=$(sed -n "${SLURM_ARRAY_TASK_ID}p" impedance_fields.txt)

# Optional: Clean up spaces for logging
SAFE_FIELD=$(echo "$FIELD" | tr ' /()' '____')
echo "Processing field: $FIELD"

# Call the main download script
bash /n/groups/patel/IGLOO/PanUKB/download_sumstats.sh "$FIELD"

echo "PanUKB Summary Stats Download Job Complete"
