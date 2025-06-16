library(dplyr)
library(reshape2)
library(ggplot2)
library(ggcorrplot)

# Load in Adiposity, demographic, bmi data

PXSconstruct <- setClass(
  "PXSconstruct",
  slots = c(
    Elist = "list", #E dataframes separated by category:
    Elist_names = "character", #Names of the category:
    Eid_cat = "data.frame", #Dataframe with environmental IDs and corresponding category.
    ordinalIDs = "character", #list environmental ordinal names
    
    UKBprot_df = "data.frame", #Protein variables dataframe
    protIDs = "character", #protein variables
    
    covars_df = "data.frame", #Covariates Dataframe
    covars_list = "character" #covars variables
  )
)

adiposity_df <- readRDS('/n/groups/patel/IGLOO/UKB/DXA/BodyAdiposity.rds')
dxa <- adiposity_df$path_124 %>%
  select(-c(android_tissue_fat_percentage_f23247_3_0, android_total_mass_f23248_3_0, arm_tissue_fat_percentage_left_f23251_3_0, arm_tissue_fat_percentage_right_f23255_3_0, 
            arm_total_mass_left_f23252_3_0, arm_total_mass_right_f23256_3_0, arms_tissue_fat_percentage_f23259_3_0, arms_total_mass_f23260_3_0, 
            gynoid_tissue_fat_percentage_f23264_3_0, gynoid_total_mass_f23265_3_0, leg_tissue_fat_percentage_left_f23268_3_0, leg_tissue_fat_percentage_right_f23272_3_0,
            leg_total_mass_left_f23269_3_0, leg_total_mass_right_f23273_3_0, trunk_tissue_fat_percentage_f23286_3_0, trunk_total_mass_f23287_3_0,
            total_tissue_fat_percentage_f23281_3_0, total_mass_f23283_3_0, legs_tissue_fat_percentage_f23276_3_0, legs_total_mass_f23277_3_0
  ))
head(dxa)



heap_df <- readRDS('/n/groups/patel/IGLOO/UKB/DXA/HEAP.rds')
demos <- heap_df@covars_df
colnames(demos)
demos <- demos %>%
  rename(sex = sex_f31_0_0, age = age_when_attended_assessment_centre_f21003_0_0, bmi = body_mass_index_bmi_f23104_0_0)
colnames(demos)

# Scale all columns of dxa data except for the eids
dxa_scaled <- scale(dxa[,2:47])
head(dxa_scaled)

dxa_scaled <- cbind(dxa$eid, dxa_scaled)
dxa_scaled <- as.data.frame(dxa_scaled)
dxa_scaled <- dxa_scaled %>% rename(eid = V1)
head(dxa_scaled)

# Scale the BMI and age data (?), don't scale sex
demos_scaled <- scale(demos[,3:4])
demos_scaled <- cbind(demos$eid, demos$sex, demos_scaled)
colnames(demos_scaled) <- c('eid', 'sex', 'bmi', 'age')
demos_scaled <- as.data.frame(demos_scaled)
head(demos_scaled)

overall <- demos_scaled %>% inner_join(dxa_scaled, by='eid')
ncol(overall)

# Now that we have the overall dataframe with demos and dxa (both scaled), 
#   we have to associate DXA with age and sex for each measure
colnames(overall)
glm_results <- lapply(names(overall)[5:50], function(adiposity) {
  # formula <- as.formula(paste(adiposity, '~ bmi + age + sex'))
  formula <- as.formula(paste(adiposity, '~ age + sex'))
  # summary(glm(formula, data = overall))$coefficients[2:4,]
  summary(glm(formula, data = overall))$coefficients[2:3,]
})
names(glm_results) <- names(overall)[5:50]

for (i in seq_along(glm_results)) {
  coef_df <- as.data.frame(glm_results[i])
  # coef_df$type <- c('bmi', 'age', 'sex')
  coef_df$type <- c('age', 'sex')
  coef_df$name <- names(glm_results)[i]
  write.table(coef_df, file='/n/groups/patel/akshaya/01_ukb/out/age_sex_dxa_assoc_NOBMI.csv', sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
}


# Finally, run an association between all of our variables
non_eid <- dxa_scaled[2:47]
names(non_eid)
cor_matrix <- cor(non_eid, use = "pairwise.complete.obs", method = "pearson")
# cor_long <- melt(cor_matrix, varnames = c("col1", "col2"), value.name = "estimate")
# write.csv(cor_long, file='/n/groups/patel/akshaya/01_ukb/out/correlation_matrix.csv', row.names=FALSE)
# 
# 
# name_conversion1 <- read.csv('/n/groups/patel/akshaya/01_ukb/src/colname_conversion.csv', header=T) %>%
#   rename(col1 = col_name, real_var1 = adiposity_type)
# name_conversion2 <- name_conversion1 %>% rename(col2 = col1, real_var2 = real_var1)
# 
# cor_long <- cor_long %>% inner_join(name_conversion1, by='col1')
# cor_long <- cor_long %>% inner_join(name_conversion2, by='col2')
# head(cor_long)
# 
# 
# head(cor_matrix)
png('/n/groups/patel/akshaya/01_ukb/out/corr_matrix.png', width=1600, height=1600)
gg1 <- ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower",
                  outline.col = "white") +  # for numbers inside tiles
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limit = c(-1, 1),
    name = expression(rho)  # for Greek ρ
    # Or use: name = "Spearman correlation"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, size = 16),  # adjust as needed
    axis.text.y = element_text(size = 16)
  )
gg1
dev.off()
