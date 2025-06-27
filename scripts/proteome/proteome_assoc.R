library(dplyr)
library(reshape2)
library(ggplot2)
library(ggcorrplot)

test <- readRDS('/n/groups/patel/akshaya/01_ukb/src/demos_dxa_proteome.rds')
names(test)
test$IID <- test$eid
test$FID <- test$eid
ncol(test)

pheno <- test[,c(3027,3026,58:103)]
covar <- test[,c(3027,3026,2:54)]

write.table(pheno, '/n/groups/patel/akshaya/01_ukb/src/ukb_pheno.txt', sep='\t', row.names=FALSE)
write.table(covar, '/n/groups/patel/akshaya/01_ukb/src/ukb_covar.txt', sep='\t', row.names=FALSE)



# Load in adiposity, demographic, bmi data
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
# impedance <- adiposity_df$path_100009
# names(impedance)
# impedance <- select(impedance, -contains("_1_0"))
# names(impedance)
# impedance <- select(impedance, -contains("_2_0"))
# names(impedance)
# impedance <- select(impedance, -contains("_3_0"))
# names(impedance)

heap_df <- readRDS('/n/groups/patel/IGLOO/UKB/DXA/HEAP.rds')
demos <- heap_df@covars_df
colnames(demos)
colnames(demos) <- c('eid', 'fasting_time','bmi',
  'age','assessment_center','pc1','pc10','pc11','pc12','pc13','pc14','pc15','pc16','pc17',
  'pc18','pc19','pc2','pc20','pc21','pc22','pc23','pc24','pc25','pc26','pc27','pc28','pc29','pc3',
  'pc30','pc31','pc32','pc33','pc34','pc35','pc36','pc37','pc38','pc39','pc4','pc40','pc5','pc6','pc7',
  'pc8','pc9','sex','age2','age_sex','age2_sex','blood_pressure_med','hormone_replacement_therapy',
  'oral_contraceptive','insulin','statin','do_not_know','none_of_the_above','prefer_not_to_answer')
colnames(demos)

proteins <- heap_df@UKBprot_df %>%
  select(-GLIPR1)

# Scale all columns of dxa data except for the eids
dxa_scaled <- scale(dxa[,2:47])
head(dxa_scaled)

dxa_scaled <- cbind(dxa$eid, dxa_scaled)
dxa_scaled <- as.data.frame(dxa_scaled)
dxa_scaled <- dxa_scaled %>% rename(eid = V1)
head(dxa_scaled)

# str(impedance[,49:148])
# Binary: impedance_device_id_f43_0_0 (46), impedance_device_id_f43_1_0 (47), impedance_device_id_f43_2_0 (48), impedance_device_id_f43_3_0 (49)

# imp_scaled_1 <- scale(impedance[,2:12])
# imp_scaled_2 <- scale(impedance[,14:39])
# 
# imp_scaled <- cbind(impedance$eid, imp_scaled_1, imp_scaled_2)
# imp_scaled <- as.data.frame(imp_scaled)
# imp_scaled <- imp_scaled %>% rename(eid = V1)
# head(imp_scaled)

# Scale the demographic data
demos_scaled_1 <- scale(demos[,2:4])
demos_scaled_2 <- scale(demos[,6:45])
demos_scaled_3 <- scale(demos[,47:49])

demos_scaled <- cbind(demos$eid, demos$assessment_center, demos$sex, demos_scaled_1, demos_scaled_2, demos_scaled_3, demos[,50:57])
colnames(demos_scaled) <- c('eid', 'assessment_center', 'sex', names(demos)[2:4], names(demos)[6:45], names(demos)[47:57])
demos_scaled <- as.data.frame(demos_scaled)
head(demos_scaled)

# Scale the protein data except for eid
proteins_scaled <- scale(proteins[,3:2924])
proteins_scaled <- cbind(proteins$eid, proteins_scaled)
proteins_scaled <- as.data.frame(proteins_scaled) %>%
  rename(eid = V1)
ncol(proteins_scaled)

head(colnames(proteins_scaled))

# overall <- demos_scaled %>% inner_join(dxa_scaled, by='eid') %>% inner_join(proteins_scaled, by='eid')
overall <- demos_scaled %>% full_join(dxa_scaled, by='eid') %>% full_join(proteins_scaled, by='eid')
nrow(overall)
head(names(overall),100)


# Now we can run our associations

adipositiesx <- names(dxa)[-1]   #103 in DXA
proteinsx  <- names(proteins)[3:2924] #3026 in DXA

results_list <- list()

non_na <- colSums(!is.na(proteins))
non_na <- as.data.frame(non_na)


counter <- 1
for (adiposity in adipositiesx) {
  print(adiposity)
  for (protein in proteinsx) {
    print(protein)
    formula <- as.formula(paste(adiposity, '~', protein, '+ age + sex + pc1 + pc2 + pc3 + pc4 + 
                               pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + assessment_center + age_sex + age2_sex'))
    model <- glm(formula, data = overall) 
    coef_summary <- summary(model)$coefficients[1,]
    
    results_list[[counter]] <- data.frame(
        Adiposity = adiposity,
        Protein = protein,
        estimate = coef_summary[1],
        sterror = coef_summary[2],
        z_value = coef_summary[3],
        p_value = coef_summary[4],
        n = length(model$fitted.values),
        stringsAsFactors = FALSE
      )
      counter <- counter + 1
    
  }
}

results_list

head(results_list, 50)

# install.packages(c("foreach", "doParallel"))
# library(foreach)
# library(doParallel)
# 
# # Detect and register cores
# n_cores <- parallel::detectCores() - 1  # Leave one core free
# cl <- makeCluster(n_cores)
# registerDoParallel(cl)
# results_list <- foreach(i = 1:length(adiposities), .combine = rbind, .packages = "stats") %:%
#   foreach(j = 1:length(proteins), .combine = rbind) %dopar% {
#     adiposity <- adiposities[i]
#     protein  <- proteins[j]
#     formula <- as.formula(paste(adiposity, '~', protein, '+ age + sex + pc1 + pc2 + pc3 + pc4 + 
#                                pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + assessment_center + age_sex + age2_sex'))
#     
#     model <- tryCatch({
#       glm(formula, data = df)
#     }, error = function(e) return(NULL))
#     
#     if (is.null(model)) return(NULL)
#     
#     coef_summary <- summary(model)$coefficients
#     out <- data.frame(
#       Adiposity = adiposity,
#       Protein = protein,
#       estimate = coef_summary[1],
#       sterror = coef_summary[2],
#       z_value = coef_summary[3],
#       p_value = coef_summary[4],
#       stringsAsFactors = FALSE
#     )
#     return(out)
#   }
# 
# stopCluster(cl)  # Stop the cluster when done
# 
# 
# 
# 
# 
# 
# 





saveRDS(overall, file='/n/groups/patel/akshaya/01_ukb/src/demos_dxa_proteome.rds')




glm_results <- do.call(rbind, results_list)

# Optional: Filter only predictor or cofactor terms (exclude intercepts)
# glm_results_filtered <- subset(glm_results, Term %in% c(predictor_vars, cofactor_vars))

# Write to CSV
write.csv(glm_results, file='/n/groups/patel/akshaya/01_ukb/out/proteome_impedance_assoc.csv', row.names = FALSE, append=TRUE)


# 
# 
# 
# glm_results <- lapply(names(overall)[104:3026], function(adiposity) {
#   formula <- as.formula(paste(adiposity, '~ age + sex + pc1 + pc2 + pc3 + pc4 + 
#                               pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + assessment_center + age_sex + age2_sex'))
#   summary(glm(formula, data = overall))$coefficients[1,]
# })
# names(glm_results) <- names(overall)[58:103]
# 
# for (i in seq_along(glm_results)) {
#   coef_df <- t(as.data.frame(glm_results[i]))
#   coef_df$name <- names(glm_results)[i]
#   write.table(coef_df, file='/n/groups/patel/akshaya/01_ukb/out/proteome_dxa_assoc.csv', sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)
# }


