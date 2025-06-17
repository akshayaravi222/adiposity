library(dplyr)
library(ggplot2)

# Make a table of the categories of exposure names
# library(purrr)
# exposure_conversion <- purrr::imap_dfr(expos, function(tbl, name) {
#   tibble(
#     category = name,
#     exposure = names(tbl)
#   )
# })
# write.csv(exposure_conversion, '/n/groups/patel/akshaya/01_ukb/src/exposure_name_conversion.csv', row.names=FALSE)



# results <- read.csv('/n/groups/patel/akshaya/01_ukb/out/batch_exposome_combined.csv', header=T)
# nrow(results)
# results <- results %>%
#   filter(Adiposity != "Adiposity")
# head(results)
# names(results) <- c('estimate', 'sterr', 't_value', 'p_value', 'Adiposity', 'n', 'exposure')
# results <- results %>% mutate_at(c('estimate', 'sterr', 't_value', 'p_value', 'n'), as.numeric)
# saveRDS(results, '/n/groups/patel/akshaya/01_ukb/out/batch_exposome_combined.rds')

results <- readRDS('/n/groups/patel/akshaya/01_ukb/out/batch_exposome_combined.rds')
nrow(results)

colname_conversion <- read.csv('/n/groups/patel/akshaya/01_ukb/src/colname_conversion.csv', header=T)
results <- results %>% full_join(colname_conversion, by='Adiposity')
head(results)

exposure_categories <- read.csv('/n/groups/patel/akshaya/01_ukb/src/exposure_name_conversion.csv', header=T)
colnames(exposure_categories)
colnames(results)
results <- results %>% full_join(exposure_categories, by='exposure')
head(results)

# Make a Manhattan Plot of results
results <- results[!is.na(results$adiposity_type),]
png('/n/groups/patel/akshaya/01_ukb/out/exposome_manhattan.png', width=2000, height=1000)
ggplot(data=results, aes(x=adiposity_type, y=-1*log10(p_value))) + 
  geom_point(size=1.5) + 
  geom_hline(yintercept = -1*log10(0.05/134459)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), 
        axis.text = element_text(size=14), 
        axis.title = element_text(size=18),
        title = element_text(size=20)) + 
  labs(x='Adiposity', title='Manhattan Plot of Adiposity x Exposome Analysis')
dev.off()

results[is.na(results$adiposity_type),]

