rm(list=ls())
library(dplyr)
library(ggplot2)
library(parallel)
library(foreach)
library(doParallel)

#####################################################
####### Model of pre-electoral manipulation #########
#####################################################

### This simulation is based on the one by Eggers et al.
### (2013) for imbalance in the US House. See their Appendix C.
### I've adapted the Stata script for Australian elections.

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
sim_range <- expand.grid(epsilon = seq(from = 0.001, to = 0.05, by = 0.0001), 
                         kappa = seq(from = 0.001, to = 0.05, by = 0.001), 
                         alpha = seq(from = 0.01, to = 0.99, by = 0.01))

# number of simulated values per arrangement of epsilon, kappa, and alpha
num_obs <- 1e6

# somewhere to store values
imbalances <- matrix(0L, nrow = num_obs, ncol = 4, 
                     dimnames = list(NULL, c("ratio","epsilon","kappa","alpha")))

imbal_fxn <- function (epsilon, kappa, alpha) {
  
  # the distribution of incumbent vote share in Australia
  signal <- rnorm(num_obs, 0.62, 0.15^2)
  error <- rnorm(num_obs, 0, epsilon)
  
  # the difference in mobilizing/not mobilizing weapon as defined above
  secret_weapon <- (pnorm( (kappa + signal - 0.5) / epsilon) - 
                      pnorm( (signal - 0.5) / epsilon)) > alpha
  
  # calculate the incumbent vote share
  vote_share <- signal + error + ( kappa * secret_weapon )
  
  # those that barely win and those that barely lose
  winning <- sum(vote_share > 0.5 & vote_share < 0.5025)
  losing <- sum(vote_share < 0.5 & vote_share > 0.4975)
  
  ratio <- winning/losing 
  return(ratio)
}

set.seed(532020)
for (i in 1:nrow(sim_range)) {
  e <- sim_range$epsilon[i]
  k <- sim_range$kappa[i]
  a <- sim_range$alpha[i]

  imbalances[i, 1] <- imbal_fxn(e, k, a)
  
  if (!is.nan(imbalances[i, 1])) {
    imbalances[i, 2] <- e
    imbalances[i, 3] <- k
    imbalances[i, 4] <- a
  }
}
 
## The imbalance seen in the data for all races won/lost 
## between 49.75 and 50.25 percent is 30:11. The odds the incumbent
## party fell into the winning bin (> 50) is 2.73 greater
## than marginally losing.

### Largest values of epsilon that produce this imbalance is 0.022
max(imbalances[imbalances[,1] >= 2.73, 2], na.rm = TRUE)


### plot epsilon against imbalance
as.data.frame(imbalances) %>% 
  ggplot(aes(x = ratio, y = epsilon)) +
  geom_point(alpha = 0.3) +
  #geom_smooth() +
  theme_minimal()

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











