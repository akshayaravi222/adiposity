library(dplyr)
library(ggplot2)
library(viridis)
library(stringr)


results <- read.csv('/n/groups/patel/akshaya/01_ukb/out/batch_proteome_combined.csv', header=T)
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



# Pathway and tissue visuals
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


tissues_output <- readRDS('/n/groups/patel/akshaya/01_ukb/out/proteome_tissues.rds')
head(tissues_output)
test_tissues <- do.call(rbind, tissues_output)
head(test_tissues)
test_tissues$adiposity_id <- rownames(test_tissues)
test_tissues <- as.data.frame(test_tissues)

adiposity_conversion <- str_split_fixed(test_tissues$adiposity_id, "_f2", 2)
colnames(adiposity_conversion) <- c('adiposity', 'junk')
adiposity_conversion <- as.data.frame(adiposity_conversion)
adiposity_conversion <- adiposity_conversion %>%
  mutate(adiposity_id = paste(adiposity, "_f2", junk, sep='')) %>%
  select(-junk)
head(adiposity_conversion)

test_tissues <- test_tissues %>% full_join(adiposity_conversion, by='adiposity_id')
head(test_tissues)


png('/n/groups/patel/akshaya/01_ukb/out/proteome_tissues.png', width=2000, height=1600)
ggplot(data=test_tissues, aes(x=Description, y=adiposity, fill=NES)) + 
  geom_tile() + 
  scale_fill_viridis(option="turbo") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90), 
        axis.text = element_text(size=14), 
        axis.title=element_text(size=16), 
        title=element_text(size=20)) + 
  labs(x='Tissue', y='Adiposity', 
       title='Tissue Analysis of Adiposities (Ranked by z-values of proteome)')
dev.off()












