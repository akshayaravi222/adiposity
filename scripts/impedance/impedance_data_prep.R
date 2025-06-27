library(dplyr)

# Load in data
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

# Load in impedance data
adiposity_df <- readRDS('/n/groups/patel/IGLOO/UKB/DXA/BodyAdiposity.rds')
impedance <- adiposity_df$path_100009
impedance <- adiposity_df$path_100009 %>%
  select(c(eid, arm_fat_free_mass_left_f23125_0_0, arm_fat_free_mass_right_f23121_0_0, arm_fat_mass_left_f23124_0_0,
           arm_fat_mass_right_f23120_0_0, arm_fat_percentage_left_f23123_0_0, arm_fat_percentage_right_f23119_0_0, arm_predicted_mass_left_f23126_0_0,
           arm_predicted_mass_right_f23122_0_0, basal_metabolic_rate_f23105_0_0, body_fat_percentage_f23099_0_0, 
           body_mass_index_bmi_f23104_0_0, impedance_of_arm_left_f23110_0_0, impedance_of_arm_right_f23109_0_0,
           impedance_of_leg_left_f23108_0_0, impedance_of_leg_right_f23107_0_0, impedance_of_whole_body_f23106_0_0,
           leg_fat_free_mass_left_f23117_0_0, leg_fat_free_mass_right_f23113_0_0, leg_fat_mass_left_f23116_0_0,
           leg_fat_mass_right_f23112_0_0, leg_fat_percentage_left_f23115_0_0, leg_fat_percentage_right_f23111_0_0,
           leg_predicted_mass_left_f23118_0_0, leg_predicted_mass_right_f23114_0_0, trunk_fat_free_mass_f23129_0_0,
           trunk_fat_mass_f23128_0_0, trunk_fat_percentage_f23127_0_0, trunk_predicted_mass_f23130_0_0,
           weight_f23098_0_0, whole_body_fat_free_mass_f23101_0_0, whole_body_fat_mass_f23100_0_0, 
           whole_body_water_mass_f23102_0_0))
impedance <- as.data.frame(impedance)

# Load in demographic data
heap_df <- readRDS('/n/groups/patel/IGLOO/UKB/DXA/HEAP.rds')
demos <- heap_df@covars_df
colnames(demos)
colnames(demos) <- c('eid', 'fasting_time','bmi',
                     'age','assessment_center','pc1','pc10','pc11','pc12','pc13','pc14','pc15','pc16','pc17',
                     'pc18','pc19','pc2','pc20','pc21','pc22','pc23','pc24','pc25','pc26','pc27','pc28','pc29','pc3',
                     'pc30','pc31','pc32','pc33','pc34','pc35','pc36','pc37','pc38','pc39','pc4','pc40','pc5','pc6','pc7',
                     'pc8','pc9','sex','age2','age_sex','age2_sex','blood_pressure_med','hormone_replacement_therapy',
                     'oral_contraceptive','insulin','statin','do_not_know','none_of_the_above','prefer_not_to_answer')

# Load in protein data
proteins <- heap_df@UKBprot_df %>%
  select(-GLIPR1)


# Scale everything
imp_scaled_1 <- scale(impedance[2:ncol(impedance)])
imp_scaled <- cbind(impedance$eid, imp_scaled_1)
imp_scaled <- as.data.frame(imp_scaled)
colnames(imp_scaled) <- c('eid', colnames(imp_scaled_1))

demos_scaled_1 <- scale(demos[,2:4])
demos_scaled_2 <- scale(demos[,6:45])
demos_scaled_3 <- scale(demos[,47:49])

demos_scaled <- cbind(demos$eid, demos$assessment_center, demos$sex, demos_scaled_1, demos_scaled_2, demos_scaled_3, demos[,50:57])
colnames(demos_scaled) <- c('eid', 'assessment_center', 'sex', names(demos)[2:4], names(demos)[6:45], names(demos)[47:57])
demos_scaled <- as.data.frame(demos_scaled)
head(demos_scaled)

# Scale the protein data except for eid
proteins_scaled <- scale(proteins[,3:ncol(proteins)])
proteins_scaled <- cbind(proteins$eid, proteins_scaled)
proteins_scaled <- as.data.frame(proteins_scaled) %>%
  rename(eid = V1)
ncol(proteins_scaled)

head(colnames(proteins_scaled))


# Output scaled dataframes
saveRDS(demos_scaled, '/n/groups/patel/akshaya/01_ukb/src/scaled_demographics.rds')
saveRDS(imp_scaled, '/n/groups/patel/akshaya/01_ukb/src/scaled_impedance.rds')
saveRDS(proteins_scaled, '/n/groups/patel/akshaya/01_ukb/src/scaled_proteins.rds')



library(purrr)
# Function to Deal with Categorical Variables
categorical_handler <- function(df, ordinal_names, ordinal_contrast = "treatment"){
  
  # :: Categorical Variables ::
  #Make some of the data categorical to help with building code:
  #*Make ordinal factors and MAKE sure these levels remain consistent in train and test*
  for(i in ordinal_names){
    df[[i]] <- factor(df[[i]], ordered = T)
    
    # Set treatment contrasts: instead of polynomial contrasts:
    #if (length(combined_levels) >= 2) {
    
    if(ordinal_contrast == "treatment"){
      contrasts(df[[i]]) <- contr.treatment(length(levels(df[[i]])))
    } else if (ordinal_contrast == "sum"){
      contrasts(df[[i]]) <- contr.sum(length(levels(df[[i]])))
    } 
  }
  
  return(df)
}

expos <- heap_df@Elist
# Break down the different exposure categories into several different tibbles
exposures_df <- expos %>% reduce(full_join)
exposures_df <- categorical_handler(exposures_df, heap_df@ordinalIDs, ordinal_contrast = "treatment")

# Scale the exposure data - excluding ordinal and binary variables
binary_colnames <- c() 
for (col_name in names(exposures_df)) {
  unique_vals <- unique(na.omit(exposures_df[[col_name]]))
  
  if (length(unique_vals) == 2) {
    binary_colnames <- c(binary_colnames, col_name)
  }
}
binary_colnames <- c(binary_colnames, 'eid')

# It turns out there's only 3 binary columns, so just put those and the ordinal ones in a separate df
binary_cols <- exposures_df[,c(binary_colnames, heap_df@ordinalIDs)]
continuous_cols <- names(exposures_df)[!names(exposures_df) %in% binary_colnames & !names(exposures_df) %in% heap_df@ordinalIDs]

scaled_exposures_cont <- exposures_df %>% select(continuous_cols)
scaled_exposures_cont <- scale(scaled_exposures_cont)
exposures_scaled <- cbind(exposures_df$eid, scaled_exposures_cont)
colnames(exposures_scaled) <- c('eid', colnames(exposures_scaled)[2:31])
exposures_scaled <- as.data.frame(exposures_scaled) %>%
  full_join(exposures_df[,c(binary_colnames, heap_df@ordinalIDs)], by='eid')

saveRDS(exposures_scaled, file='/n/groups/patel/akshaya/01_ukb/src/scaled_exposures.rds')
