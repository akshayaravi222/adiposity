library(dplyr)
library(data.table)
library(biomaRt)

sum_stats <- fread("zcat '/n/groups/patel/IGLOO/PanUKB/SumStats/Arm_fat_mass_(left)_23124_both_sexes.tsv.bgz'", sep = "\t", header = TRUE)
sum_stats <- sum_stats %>%
  filter(neglog10_pval_meta_hq > 7.301) %>%
  rename(chrom = chr) %>%
  mutate(location = paste(chrom, pos, pos, sep=':'))
# saveRDS(sum_stats, '/n/groups/patel/akshaya/01_ukb/src/arm_fat_mass_left_filtered.rds')


rsids <- fread("zcat '/n/groups/patel/IGLOO/PanUKB/SumStats/full_variant_qc_metrics.txt.bgz'", sep = "\t", header = TRUE)
rsids_only <- rsids[,c(1,2,5)]
rsids_only <- rsids_only %>% 
  mutate(location = paste(chrom, pos, pos, sep=':'))
rsids_only <- distinct(rsids_only)

sum_stats <- sum_stats %>% left_join(rsids_only, by=c('location'))





## Use the default ENSEMBL Variation Mart & Human dataset
snpMart = useEnsembl(biomart = "snps", 
                     dataset = "hsapiens_snp")

## Make a new column of our data in the format chr:start:end
## It's important to include the end even if it's a single base, 
## otherwise it searches to the end of the chromosome
sum_stats <- sum_stats %>%
  mutate(location = paste(chr, pos, pos, sep=':'))

## Submit the query
rsids <- getBM(attributes = c('refsnp_id', 'chr_name', 'chrom_start', 'chrom_end', 'allele'),
      filters = c('chromosomal_region'),
      values = sum_stats$location,
      mart = snpMart)
# #>     refsnp_id chr_name chrom_start chrom_end allele
# #> 1 rs775809821        1       10020     10021   AA/A
# #> 2 rs978760828        1       10039     10039    A/C
# 

n <- nrow(sum_stats)

# args = commandArgs(trailingOnly = T)
batch_size <- 1000
# i <- as.integer(args[1]) * 1000
i <- 1
batch <- readRDS('/n/groups/patel/akshaya/01_ukb/src/arm_fat_mass_left_filtered.rds')
batch <- batch[i:(i+batch_size-1),]
# TODO: PRINT THE BATCH NUMBER
rsids <- getBM(attributes = c('refsnp_id', 'chr_name', 'chrom_start', 'chrom_end', 'allele'),
               filters = c('chromosomal_region'),
               values = batch$location,
               mart = snpMart)




for (i in seq(1, n, by = batch_size)) {
  message(sprintf("Processing batch %d of %d", ceiling(i / batch_size), ceiling(n / batch_size)))
  
  batch <- sum_stats[i:min(i + batch_size - 1, n)]
  
  res <- tryCatch({
    getBM(
      attributes = c('refsnp_id', 'chr_name', 'chrom_start', 'chrom_end', 'allele'),
      filters = c("chromosomal_region"),
      values = batch$location,
      mart = snpMart
    )
  }, error = function(e) data.table())  # Handle biomaRt timeout or NULL response
  
  results_list[[ceiling(i / batch_size)]] <- res
}

# Combine all batches
snp_annotations <- rbindlist(results_list, fill = TRUE)
head(snp_annotations)
setnames(snp_annotations, c("refsnp_id", "chr", "position"))







