library(dplyr)

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

demos <- readRDS('/n/groups/patel/akshaya/01_ukb/src/scaled_demographics.rds')
impedance <- readRDS('/n/groups/patel/akshaya/01_ukb/src/scaled_impedance.rds')
exposures <- readRDS('/n/groups/patel/akshaya/01_ukb/src/scaled_exposures.rds')

overall <- impedance %>% full_join(demos, by='eid') %>% full_join(exposures, by='eid')
heap_df <- readRDS('/n/groups/patel/IGLOO/UKB/DXA/HEAP.rds')
overall <- categorical_handler(overall, heap_df@ordinalIDs, "treatment")

# Obtain arguments from bash script:
args = commandArgs(trailingOnly = T)
adiposity_col <- as.integer(args[1])

# i loops between 2 and 33
adiposity <- names(overall)[adiposity_col]

results_list <- data.frame()
counter <- 1
omit_cols <- c('bread_type_f1448_0_0_Prefer_not_to_answer', 
               'major_dietary_changes_in_the_last_5_years_f1538_0_0_Prefer_not_to_answer',
               'milk_type_used_f1418_0_0_Prefer_not_to_answer', 'milk_type_used_f1418_0_0_Do_not_know',
               'types_of_physical_activity_in_last_4_weeks_f6164_0_0.multi_Prefer_not_to_answer',
               'types_of_transport_used_excluding_work_f6162_0_0.multi_Prefer_not_to_answer')
overall <- overall %>% select(-all_of(omit_cols))
exposure_names  <- names(overall)[90:ncol(overall)]

for (exposure in exposure_names) {
  print(exposure)
  formula <- as.formula(paste(adiposity, '~', exposure, '+ age + sex + pc1 + pc2 + pc3 + pc4 + 
                             pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + assessment_center + age_sex + age2_sex'))
  model <- lm(formula, data = overall) 
  coef_summary <- summary(model)$coefficients
  coef_summary <- as.data.frame(coef_summary)
  coef_summary$Adiposity = adiposity
  coef_summary$n = length(model$fitted.values)
  coef_summary <- coef_summary[grepl(exposure, rownames(coef_summary)),]
  results_list = rbind(results_list, coef_summary)
  
  # results_list[[counter]] <- data.frame(
  #   Adiposity = adiposity,
  #   Exposure = exposure,
  #   estimate = coef_summary[1],
  #   sterror = coef_summary[2],
  #   z_value = coef_summary[3],
  #   p_value = coef_summary[4],
  #   n = length(model$fitted.values),
  #   stringsAsFactors = FALSE
  # )
  counter <- counter + 1
  
}

results_list$exposure <- rownames(results_list)
write.csv(results_list, file=paste('/n/groups/patel/akshaya/01_ukb/out/slurm_imp_exposome_assoc/batch_imp_exposome_', adiposity_col, sep=''), row.names = FALSE)
