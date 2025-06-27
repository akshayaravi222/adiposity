library(dplyr)
library(ggplot2)
library(table1)

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


demos_dxa <- dxa %>% left_join(demos, by='eid')

table1(~ factor(sex) + bmi + age + factor(statin) + factor(blood_pressure_med) + factor(insulin), data=demos_dxa)

means <- demos_dxa %>%
  group_by(age, sex) %>%
  summarise(across(2:47, median, na.rm = TRUE))
long_means <- gather(means, key='measurement', value='median', -c(age, sex))
head(long_means)

png('/n/groups/patel/akshaya/01_ukb/out/age_by_dxa_plots.png', width=2000, height=2000)
ggplot(data=long_means, aes(x=age, y=median, color=sex)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~ measurement, scales='free')
dev.off()






