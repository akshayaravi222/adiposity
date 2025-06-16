library(ggplot2)
library(dplyr)
library(corrplot)

sex <- read.csv('/n/groups/patel/akshaya/01_ukb/src/dxa_sex_results.csv', header=T)
sex <- sex %>%
  mutate(low = estimate-stdev) %>%
  mutate(high=estimate+stdev)
head(sex)

# volcano plot doesn't work well because so many estimates are 0
# ggplot(data=sex, aes(x=volcano_estimate, y=volcano_p)) + geom_point()

# Cochrane data from the 'rmeta'-package

png('/n/groups/patel/akshaya/01_ukb/out/dxa_sex.png', height=2000, width=800)
ggplot(data=sex, aes(x=estimate, y=adiposity_type, color=significant)) + 
  facet_grid(body_part ~ ., scales = "free_y", space = "free_y") + 
  scale_color_manual(values = c("darkgreen", "red")) +
  geom_point(size=3) + 
  geom_linerange(aes(xmin=estimate-stdev, xmax=estimate+stdev), linewidth=1) + 
  labs(x = 'Estimate', y = 'Adiposity Measure', title='DXA Association with Sex') + 
  geom_vline(xintercept = 0, lty=2, lwd=1.5) + 
  theme_light() + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), strip.text = element_text(size = 16),
        axis.text.x = element_text(size=12), title=element_text(size=20), 
        legend.position="none")
dev.off()




# age

age <- read.csv('/n/groups/patel/akshaya/01_ukb/src/dxa_age_results.csv', header=T)
age <- age %>%
  mutate(low = estimate-stdev) %>%
  mutate(high=estimate+stdev)
head(age)

png('/n/groups/patel/akshaya/01_ukb/out/dxa_age.png', height=2000, width=800)
ggplot(data=age, aes(x=estimate, y=adiposity_type, color=significant)) + 
  facet_grid(body_part ~ ., scales = "free_y", space = "free_y") + 
  scale_color_manual(values = c("red", "darkgreen")) +
  geom_point(size=3) + 
  geom_linerange(aes(xmin=estimate-stdev, xmax=estimate+stdev), linewidth=1) + 
  labs(x = 'Estimate', y = 'Adiposity Measure', title='DXA Association with Age') + 
  geom_vline(xintercept = 0, lty=2, lwd=1.5) + 
  theme_light() + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), strip.text = element_text(size = 16),
        axis.text.x = element_text(size=12), title=element_text(size=20), 
        legend.position="none")
dev.off()



# bmi

bmi <- read.csv('/n/groups/patel/akshaya/01_ukb/src/dxa_bmi_results.csv', header=T)
bmi <- bmi %>%
  mutate(low = estimate-stdev) %>%
  mutate(high=estimate+stdev)
head(bmi)

png('/n/groups/patel/akshaya/01_ukb/out/dxa_bmi.png', height=2000, width=800)
ggplot(data=bmi, aes(x=estimate, y=adiposity_type, color=significant)) + 
  facet_grid(body_part ~ ., scales = "free_y", space = "free_y") + 
  scale_color_manual(values = c("darkgreen", "red")) +
  geom_point(size=3) + 
  geom_linerange(aes(xmin=estimate-stdev, xmax=estimate+stdev), linewidth=1) + 
  labs(x = 'Estimate', y = 'Adiposity Measure', title='DXA Association with BMI') + 
  geom_vline(xintercept = 0, lty=2, lwd=1.5) + 
  theme_light() + 
  theme(axis.text.y = element_text(size=12), axis.title.y = element_text(size=20), 
        axis.title.x = element_text(size=20), strip.text = element_text(size = 16),
        axis.text.x = element_text(size=12), title=element_text(size=20), 
        legend.position="none")
dev.off()

