library(dplyr)

bmi_proteome_df <- read.table('/n/groups/patel/akshaya/01_ukb/out/bmi_batch_combined.txt', header=F, sep=',')
colnames(bmi_proteome_df) <- c("Adiposity","Protein","estimate","sterror","z_value","p_value","n")

