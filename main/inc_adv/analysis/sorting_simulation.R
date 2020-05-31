rm(list=ls())
library(dplyr)
library(ggplot2)
# library(parallel)
# library(foreach)
# library(doParallel)

#####################################################
####### Model of pre-electoral manipulation #########
#####################################################

### This simulation is based on the one by Eggers et al.
### (2013) for imbalance in the US House. See their Appendix C.
### I've adapted the Stata script for Australian elections.
### Takes a while (esp if num_obs > 1e4). Best to use multiple cores.

## inc vote share = signal + error + ( k * secret weapon )
# signal ~ N(0.62, 0.15^2)
# error ~ N(0, e^2)
# `secret weapon` is a binary indicator of whether 
# incumbent chooses to deploy the secret weapon

# GOAL: find the largest possible value of `e` that would
# produce the imbalance observed in the data.

## incumbent's probability of victory: 
# = Pr[signal + error + ( k * secret weapon ) > 0.5]
# = Pr[error > 0.5 - signal - ( k * secret weapon )]
# = Pr[error < ( k * secret weapon ) - 0.5 + signal]
# = Normal_cdf( [( k * secret weapon ) - 0.5 + signal] / error )

## If an incumbent calculates (def. Pi) their chance of victory by 
## differencing the probability of victory with and without 
## deploying the secret weapon, and will only use it if the 
## difference is greater than the cost of deploying the secret
## weapon (def. alpha):

# Pi = Normal_cdf( [k - 0.5 + signal] / error ) -
# Normal_cdf( [- 0.5 + signal] / error ) > alpha

## Substituting this equality into the primary model gives:
# inc vote share = signal + error + ( k * 1{Pi} )

# define possible ranges
sim_range <- expand.grid(epsilon = seq(from = 1e-3, to = 0.05, by = 1e-4), 
                         kappa = seq(from = 1e-3, to = 0.05, by = 1e-3), 
                         alpha = seq(from = 1e-2, to = 0.99, by = 1e-2))

# number of simulated values per arrangement of epsilon, kappa, and alpha
num_obs <- 1e5

# somewhere to store values
imbalances <- matrix(0L, nrow = num_obs, ncol = 5, 
                     dimnames = list(NULL, c("ratio","epsilon","kappa","alpha","error")))

# create a progress bar -- not working
# same with txtProgressBar :(
# pb <- tkProgressBar("Simulation Progress", "Simulating...",
#                     0, 100, initial = 0)

set.seed(532020)
for (i in 1:nrow(sim_range)) {
  
  # the distribution of incumbent vote share in Australia
  signal <- rnorm(num_obs, 0.58, 0.067)
  error <- rnorm(num_obs, 0, sim_range$epsilon[i])
  
  # the difference in mobilizing/not mobilizing weapon as defined above
  secret_weapon <- (pnorm( (sim_range$kappa[i] + signal - 0.5) / sim_range$epsilon[i]) - 
                      pnorm( (signal - 0.5) / sim_range$epsilon[i])) > sim_range$alpha[i]
  
  # calculate the incumbent vote share
  vote_share <- signal + error + ( sim_range$kappa[i] * secret_weapon )
  
  # those that barely win and those that barely lose
  winning <- sum(vote_share > 0.5 & vote_share < 0.5025)
  losing <- sum(vote_share < 0.5 & vote_share > 0.4975)
  
  imbalances[i, 1] <- winning/losing 
  
  if (!is.nan(imbalances[i, 1]) & !is.infinite(imbalances[i, 1])) {
    imbalances[i, 2] <- sim_range$epsilon[i]
    imbalances[i, 3] <- sim_range$kappa[i]
    imbalances[i, 4] <- sim_range$alpha[i]
    imbalances[i, 5] <- mean(error, na.rm = T)
  }
  # Sys.sleep(1)
  # setTkProgressBar(pb, i)
}
# close(pb)

### Largest values of epsilon that produce imbalance of 2.73 is 0.0011
max(imbalances[imbalances[,1] >= 2.73, 2], na.rm = TRUE)


as.data.frame(imbalances) %>% 
  filter(ratio >= 2.73)%>% 
  arrange(desc(epsilon))


### plot error against imbalance
as.data.frame(imbalances) %>% 
  ggplot(aes(x = abs(error), y = ratio)) +
  geom_point(alpha = 0.2) +
  geom_point(inherit.aes = FALSE, aes(y = 2.73, x = 2.390450e-06), 
             col = "red", shape = 2) +
  labs(x = "| Error |", y = "Win/Loss Imbalance for Incumbents in Close Races",
      title = "Simulated Level of Precision Producing\n Imbalances in Close Australian Elections") + 
  #geom_smooth() +
  #geom_vline(xintercept = 2.390450e-06, col = "red", lty = 2) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#####################################################
########## run in parallel to speed it up ###########
#####################################################

# use multicore, set to the number of our cores
num_cores <- detectCores()- 1
registerDoParallel(num_cores)


# foreach works more like lapply, less like for loop
tmp <- foreach(i=1:nrow(sim_range), .combine = 'c') %dopar% {
  
    epsilon <- sim_range$epsilon[i]
    kappa <- sim_range$kappa[i]
    alpha <- sim_range$alpha[i]
    
    imbalances[i, 1] <- imbal_fxn(epsilon, kappa, alpha)

}


# stop clustering after sim
stopCluster(cl)









