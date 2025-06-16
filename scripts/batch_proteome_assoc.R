library(dplyr)


overall <- readRDS('/n/groups/patel/akshaya/01_ukb/src/demos_dxa_proteome.rds')

# Obtain arguments from bash script:
args = commandArgs(trailingOnly = T)
i <- as.integer(args[1])

# i loops between 58 and 103
adiposity <- names(overall)[i]
protein_names  <- names(overall)[104:3025] #3025

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
write.csv(glm_results, file=paste('/n/groups/patel/akshaya/01_ukb/out/slurm_proteome_assoc/batch_proteome_', i), row.names = FALSE)
