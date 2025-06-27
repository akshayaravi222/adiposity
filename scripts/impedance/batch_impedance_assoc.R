library(dplyr)


demos <- readRDS('/n/groups/patel/akshaya/01_ukb/src/scaled_demographics.rds')
impedance <- readRDS('/n/groups/patel/akshaya/01_ukb/src/scaled_impedance.rds')
proteins <- readRDS('/n/groups/patel/akshaya/01_ukb/src/scaled_proteins.rds')

overall <- impedance %>% full_join(demos, by='eid') %>% full_join(proteins, by='eid')

# Obtain arguments from bash script:
args = commandArgs(trailingOnly = T)
i <- as.integer(args[1])

# i loops between 2 and 33
adiposity <- names(overall)[i]
protein_names  <- names(overall)[90:ncol(overall)]

results_list <- list()

counter <- 1

for (protein in protein_names) {
  print(protein)
  formula <- as.formula(paste(adiposity, '~', protein, '+ age + sex + pc1 + pc2 + pc3 + pc4 + 
                             pc5 + pc6 + pc7 + pc8 + pc9 + pc10 + assessment_center + age_sex + age2_sex'))
  model <- glm(formula, data = overall) 
  coef_summary <- summary(model)$coefficients[protein,]
  
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

glm_results <- do.call(rbind, results_list)
write.csv(glm_results, file=paste('/n/groups/patel/akshaya/01_ukb/out/slurm_imp_proteome_assoc/batch_imp_proteome_', i, sep=''), row.names = FALSE)
