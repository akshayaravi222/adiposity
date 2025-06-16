library(dplyr)
library(ggplot2)
library(viridis)


results <- read.csv('/n/groups/patel/akshaya/01_ukb/out/slurm_proteome_assoc/batch_proteome_combined.csv', header=T)
nrow(results)
results <- results %>%
  filter(Adiposity != "Adiposity")
head(results)

colname_conversion <- read.csv('/n/groups/patel/akshaya/01_ukb/src/colname_conversion.csv', header=T)
results <- results %>% full_join(colname_conversion, by='Adiposity')
head(colname_conversion)

results$p_value <- as.numeric(results$p_value)

head(results)

png('/n/groups/patel/akshaya/01_ukb/out/proteome_manhattan.png', width=2000, height=1000)
ggplot(data=results, aes(x=adiposity_type, y=-1*log10(p_value), color=body_part)) + 
  geom_point(size=1.5) + 
  geom_hline(yintercept = -1*log10(0.05/134459)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), axis.text = element_text(size=14), axis.title = element_text(size=18))
dev.off()



# Pathway visuals
pathways_output <- readRDS('/n/groups/patel/akshaya/01_ukb/out/proteome_pathways.rds')
head(pathways_output)
test_pathways <- do.call(rbind, pathways_output)
head(test_pathways)
test_pathways$adiposity_id <- rownames(test_pathways)
test_pathways <- as.data.frame(test_pathways)
adiposity_name_conversion <- read.csv('/n/groups/patel/akshaya/01_ukb/src/adiposity_name_conversion.csv', header=T)
head(adiposity_name_conversion)
test_pathways <- test_pathways %>% full_join(adiposity_name_conversion, by='adiposity_id')
head(test_pathways)


png('/n/groups/patel/akshaya/01_ukb/out/proteome_pathways.png', width=2000, height=1600)
ggplot(data=test_pathways, aes(x=Description, y=adiposity, fill=NES)) + 
  geom_tile() + 
  scale_fill_viridis(option="turbo") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90), 
        axis.text = element_text(size=14), 
        axis.title=element_text(size=16), 
        title=element_text(size=20)) + 
  labs(x='Pathway', y='Adiposity', 
       title='Pathway Analysis of Adiposities (Ranked by z-values of proteome)')
dev.off()

# head(test)
# 
# nes_only <- test_pathways %>% select(c('Description', 'adiposity', 'NES'))
# wide_pathways <- reshape(nes_only, idvar = "Description", timevar = "adiposity", direction = "wide")
# head(wide_pathways)
# 
# library(ggcorrplot)
# wide_pathways[,2:33] <- sapply(wide_pathways[,2:33],as.numeric)
# head(wide_pathways)
# library(pheatmap)

# png('/n/groups/patel/akshaya/01_ukb/out/proteome_pathways_2.png', width=1600, height=1600)
# ggcorrplot(wide_pathways, hc.order = TRUE, type = "lower",
#                   outline.col = "white") +  # for numbers inside tiles
#   scale_fill_gradient2(
#     low = "blue", mid = "white", high = "red",
#     midpoint = 0, limit = c(-10, 10),
#     name = expression(rho)  # for Greek ρ
#     # Or use: name = "Spearman correlation"
#   ) +
#   theme(
#     axis.text.x = element_text(angle = 45, size = 16),  # adjust as needed
#     axis.text.y = element_text(size = 16)
#   )
# dev.off()












