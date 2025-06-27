library(dplyr)
library(purrr)

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




heap_df <- readRDS('/n/groups/patel/IGLOO/UKB/DXA/HEAP.rds')
expos <- heap_df@Elist
head(expos)

# Break down the different exposure categories into several different tibbles
exposures_df <- expos %>% reduce(full_join)
exposures_df <- categorical_handler(exposures_df, heap_df@ordinalIDs, ordinal_contrast = "treatment")


# Input the DXA scan data
adiposity_df <- readRDS('/n/groups/patel/IGLOO/UKB/DXA/BodyAdiposity.rds')
dxa <- adiposity_df$path_124 %>%
  select(-c(android_tissue_fat_percentage_f23247_3_0, android_total_mass_f23248_3_0, arm_tissue_fat_percentage_left_f23251_3_0, arm_tissue_fat_percentage_right_f23255_3_0, 
            arm_total_mass_left_f23252_3_0, arm_total_mass_right_f23256_3_0, arms_tissue_fat_percentage_f23259_3_0, arms_total_mass_f23260_3_0, 
            gynoid_tissue_fat_percentage_f23264_3_0, gynoid_total_mass_f23265_3_0, leg_tissue_fat_percentage_left_f23268_3_0, leg_tissue_fat_percentage_right_f23272_3_0,
            leg_total_mass_left_f23269_3_0, leg_total_mass_right_f23273_3_0, trunk_tissue_fat_percentage_f23286_3_0, trunk_total_mass_f23287_3_0,
            total_tissue_fat_percentage_f23281_3_0, total_mass_f23283_3_0, legs_tissue_fat_percentage_f23276_3_0, legs_total_mass_f23277_3_0
  ))
head(dxa)

# Input the demographic data
demos <- heap_df@covars_df
colnames(demos) <- c('eid', 'fasting_time','bmi',
                     'age','assessment_center','pc1','pc10','pc11','pc12','pc13','pc14','pc15','pc16','pc17',
                     'pc18','pc19','pc2','pc20','pc21','pc22','pc23','pc24','pc25','pc26','pc27','pc28','pc29','pc3',
                     'pc30','pc31','pc32','pc33','pc34','pc35','pc36','pc37','pc38','pc39','pc4','pc40','pc5','pc6','pc7',
                     'pc8','pc9','sex','age2','age_sex','age2_sex','blood_pressure_med','hormone_replacement_therapy',
                     'oral_contraceptive','insulin','statin','do_not_know','none_of_the_above','prefer_not_to_answer')

# Scale all columns of dxa data except for the eids
dxa_scaled <- scale(dxa[,2:47])
head(dxa_scaled)

dxa_scaled <- cbind(dxa$eid, dxa_scaled)
dxa_scaled <- as.data.frame(dxa_scaled)
dxa_scaled <- dxa_scaled %>% rename(eid = V1)
head(dxa_scaled)

# Scale the demographic data
demos_scaled_1 <- scale(demos[,2:4])
demos_scaled_2 <- scale(demos[,6:45])
demos_scaled_3 <- scale(demos[,47:49])

demos_scaled <- cbind(demos$eid, demos$assessment_center, demos$sex, demos_scaled_1, demos_scaled_2, demos_scaled_3, demos[,50:57])
colnames(demos_scaled) <- c('eid', 'assessment_center', 'sex', names(demos)[2:4], names(demos)[6:45], names(demos)[47:57])
demos_scaled <- as.data.frame(demos_scaled)
head(demos_scaled)

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



overall <- demos_scaled %>% full_join(dxa_scaled, by='eid') %>% full_join(exposures_scaled, by='eid')
# overall[,binary_colnames] <- as.factor(overall[,binary_colnames])

saveRDS(overall, file='/n/groups/patel/akshaya/01_ukb/src/demos_dxa_exposures.rds')





