library(tidyverse)
library(psych)

#####################################################
##### Descriptive statistics [input clean data] #####
#####################################################

tpp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tpp_data.csv")

theme_set(
  theme_light()
)

#---------- descriptive stats ----------# 

covariates <- c("fp_vote_share", "Swing", "alp_vs", "lnp_vs", 
                "alp_margin_t", "lnp_margin_t", "open_seat", 
                "alp_incumbent", "lnp_incumbent")

tpp_data %>% 
  select(covariates) %>% 
  describe()

# plots
map(covariates, function (x) {
  qplot(tpp_data[[x]], geom = "density") +
    labs(x = x)
  })


#---------- check incumbency imbalance ----------# 


tpp_data %>% 
  filter(alp_incumbent == 1) %>% 
ggplot() +
 geom_histogram(aes(alp_margin_t), bins = 100,
                fill = "red", alpha = 0.5) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  labs(x = "Vote share")

tpp_data %>% 
  filter(lnp_incumbent == 1) %>% 
  ggplot() +
  geom_histogram(aes(alp_margin_t), bins = 100,
                 fill = "blue", alpha = 0.5) +
  scale_x_continuous(limits = c(-0.5, 0.5)) +
  labs(x = "Vote share")

#---------- vote share for different parties over time ----------# 





















