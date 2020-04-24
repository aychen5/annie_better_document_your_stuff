library(tidyverse)

#####################################################
##### Descriptive statistics [input clean data] #####
#####################################################


#---------- check imbalance ----------# 

#66
table(all_data$incumbent)
#61
all_data %>% 
  filter(incumbent == 1 & alp_win_t1 == 1) %>% 
  nrow()

ggplot(all_data[all_data$incumbent == 1,]) +
  geom_density(aes(alp_vs)) +
  geom_vline(xintercept = mean(all_data[all_data$incumbent == 1,]$alp_vs, na.rm = T), 
             lty = 2, col = "red", lwd = 1.5) + 
  theme_minimal()

#mean
57.98788/100
#sd
6.695208/100

