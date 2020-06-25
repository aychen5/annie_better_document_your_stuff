pacman::p_load(dplyr, ggplot2, rdrobust, rdd, readr)


### ------------ANNIE'S TO-DO's --------------- ###
# - tests: 
#   + density
#   + bandwidth
#   + kernel
#   + placebo cutoffs
# - covariates:
#   + donations, spending
#   + turnout
#   + partisanship of division

###############################################################
######### RDD Analysis (National; Party) ##########
###############################################################
# finally getting to use data painstaking cleansed (=^･ｪ･^=))ﾉ彡☆


### --------------- PARTY-LEVEL ANALYSIS ----------------- ###
tpp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tpp_data.csv")

my_rdd_fxn <- function (outcome, running, data = tpp_data, 
                        covars = NULL, h = NULL) {
        
        attach(data, warn.conflicts = FALSE)
        
        mod <- rdrobust(y = outcome,
                 # alp margin of victory at t
                 x = running,
                 kernel = "triangular",
                 bwselect = 'msetwo',
                 h = h,
                 c = 0.5, p = 1,
                 covs = covars,
                 all = TRUE,
                 cluster = DivisionNm)
        
        out <- summary(mod)
        return(out)
}


# this is party-level data
#vote share in next election
party_mod1 <- my_rdd_fxn(outcome = alp_vs_t1, running = alp_vs)
party_mod2 <- my_rdd_fxn(outcome = alp_win_t1, running = alp_vs)

# use fp vote share as the outcome 
party_fp_mod <- my_rdd_fxn(outcome = alp_fp_t1, running = alp_vs)

# LNP instaed of ALP
party_lnp_mod <- my_rdd_fxn(outcome = lnp_fp_t1, running = lnp_margin_t)

# manual bandwidths
party_bw_mod <- my_rdd_fxn(outcome = alp_fp_t1, running = alp_margin_t,
                           h = 0.05)

# Add covariates 
covars <- c(open_seat)
party_covs_mod <- my_rdd_fxn(outcome = alp_fp_t1, running = alp_margin_t, covars = covars)


