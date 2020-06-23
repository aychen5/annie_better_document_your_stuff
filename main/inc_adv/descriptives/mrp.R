## ----loadlibs------------------------------------------------------------
library(sjlabelled)
library(foreign)
library(readxl)
library(tidyverse)
library(rstanarm)
library(Formula)
library(parallel)
library(tidybayes)

####################################################################
# This script performs Multilevel Regression and Poststratification 
# to get division-level political ideology scores.
# The poststratification frame is takes variables from the Australian
# Bureau of Statistics' 2016 Census. 
# I use the 2019 Australian Voter Survey's ideology question, where 
# 1 = left -> 10 = right.
###################################################################


## ----getpsw--------------------------------------------------------------

# this post-strat table is: age group x education x gender x marital status
# 7 x 5 x 2 x 4 levels for each of 150 divisions = 42000 possible levels

au_psw <- read_csv("~/Dropbox/Thesis/inc_adv/raw_data/census/poststrat_frame.csv",
                   skip = 9) %>% 
    rename(division = `CED (UR)`,
           age = `AGE10P - Age in Ten Year Groups`,
           education = `HEAP - 1 Digit Level`,
           gender = `SEXP Sex`,
           married = `MSTP Registered Marital Status`,
           freq = `X6`) %>% 
    fill(c(division, age, education, gender), .direction = "down") %>% 
    #remove irrelevant years
    filter(!age %in% list("0-9 years", "10-19 years", "90-99 years",  "100 years and over", "Total") &
               education !="Graduate Diploma and Graduate Certificate Level") %>% 
    # remove weird rows
    filter(!grepl("Offshore|No usual address|Creative Commons|Copyright|Total|INFO", division)) 

# merge with population data for each division

au_pop <- read_csv("~/Dropbox/Thesis/inc_adv/raw_data/census/division_pop.csv", 
                   skip = 9) %>% 
    rename(division = `CED (UR)`,
           pop = X2) %>% 
    mutate(pop = as.numeric(pop))%>% 
    filter(!grepl("Offshore|No usual address|Creative Commons|Copyright|Total|INFO", division)) 

grpid = function(x) match(x, unique(x))
au_psw <- au_psw %>% 
    left_join(au_pop, by = "division") %>% 
    mutate(pop_weight = freq/pop,
           # need to account for redistricting from 2016-2019
           division = case_when(division == "Wakefield" ~ "Spence",
                                division == "Denison" ~ "Clark",
                                division == "Batman" ~ "Cooper",
                                division == "McMillan" ~ "Monash",
                                division == "Melbourne Ports" ~ "Macnamara",
                                division == "Murray" ~ "Nicholls",
                                TRUE ~ division)) %>%
    mutate(id = group_indices(., division) %>% grpid) 

View(au_psw)

## ---- getbesdata ----------------------------------------------------------

## Read in and select the variables we want
mrp_data_fxn <- function (data) {
    attach(data)
    out <- data.frame(
        age = AGE,
        gender = dem_gen_16,
        religion = H6,
        married = H8,
        education = G3,
        income = J6,
        housing = dem_home,
        state = STATE,
        division = CED_AEC,
        pid = B1,
        pid_strendth = B2,
        ideology = B8_1,
        weights = wt_pooled
    )
    return(out)
}

# recode variables match poststratification frame
aes <- mrp_data_fxn(data$AES_2019) %>% 
    mutate(gender = ifelse(gender == 1, "Male", "Female"),
           age = case_when(age >= 20 & age <= 29 ~ "20-29 years",
                           age >= 30 & age <= 39 ~ "30-39 years",
                           age >= 40 & age <= 49 ~ "40-49 years",
                           age >= 50 & age <= 59 ~ "50-59 years",
                           age >= 60 & age <= 69 ~ "60-69 years",
                           age >= 70 & age <= 79 ~ "70-79 years",
                           age >= 80 & age <= 89 ~ "80-89 years"),
           education = case_when(education == 1 ~ "Secondary Education - Years 9 and below",
                                 education == 2 ~ "Postgraduate Degree Level",
                                 education == 3 ~ "Bachelor Degree Level",
                                 education %in% list(4, 5) ~ "Advanced Diploma and Diploma Level",
                                 education %in% list(6, 7) ~ "Secondary Education - Years 10 and above"),
           married = case_when(married == 1 ~ "Never married",
                               married == 2 ~ "Married",
                               married == 3 ~ "Widowed",
                               married == 4 ~ "Divorced"),
           pid = factor(case_when(pid == 1 ~ "Liberal",
                           pid == 2 ~ "Labor",
                           pid == 3 ~ "National Party",
                           pid == 4 ~ "Greens"),
                           levels = c("Greens", "Labor", "Liberal", "National Party")),
           ideology = case_when(ideology == "Left" ~ 0,
                                ideology == "1" ~ 1,
                                ideology == "2" ~ 2,
                                ideology == "3" ~ 3,
                                ideology == "4" ~ 4,
                                ideology == "5" ~ 5,
                                ideology == "6" ~ 6,
                                ideology == "7" ~ 7,
                                ideology == "8" ~ 8,
                                ideology == "9" ~ 9,
                                ideology == "Right" ~ 10),
           division = as.character(as_label(division))
           ) %>% 
    filter(!division %in% list("Bean", "Unknown", "Fraser")) %>% # abolished divs in 2019 
    merge(unique(select(au_psw, c(division, id))), by = "division")

#quick checks
setdiff(aes$division, au_psw$division)
setdiff(au_psw$division, aes$division)
setdiff(au_psw$division, aux$division)
setdiff(aux$division, au_psw$division)

View(aes)

#aes <- aes[complete.cases(aes),]

## ----auxilliary data-----------------------------------------------------------
# use this as fixed effects 
citizenship <- readxl::read_xlsx("~/Dropbox/Thesis/inc_adv/raw_data/census/aux_citizen.xlsx",
                                 skip = 8) %>% 
    rename(division = `CED (UR)`,
           citizenship = `CITP Australian Citizenship`,
           freq = ...3) %>% 
    fill(division, .direction = "down") %>% 
    pivot_wider(names_from = citizenship, values_from = freq) %>% 
    filter(!grepl("Offshore|No usual address", division)) %>%
    slice(1:(n()-6)) %>% 
    select(division, Australian, `Not Australian`) %>% 
    mutate(Australian = as.numeric(Australian))

aux <- au_pop %>% 
    right_join(citizenship, by = "division") %>% 
    mutate(citizen = Australian/pop,
           # need to account for redistricting from 2016-2019
           division = case_when(division == "Wakefield" ~ "Spence",
                                division == "Denison" ~ "Clark",
                                division == "Batman" ~ "Cooper",
                                division == "McMillan" ~ "Monash",
                                division == "Melbourne Ports" ~ "Macnamara",
                                division == "Murray" ~ "Nicholls",
                                TRUE ~ division))

# merge iwth survey 
aes_aux <- merge(aes, aux,
              by = "division",
              all.x = TRUE,
              all.y = FALSE)
psw_aux <- merge(au_psw, aux,
                 by = "division",
                 all.x = TRUE,
                 all.y = FALSE)

## ----domrp, cache = FALSE------------------------------------------------

# use a multi-level logistic regression model to predict political ideology
# in the sample given the variables that we will use to post-stratify

#Varying intercept model with no predictors
# denote
# \theta_j = (X_j\beta)

# random intercepts
# \alpha_{division_[j]} + \alpha_{age_[j]} + \alpha_{education_[j]} + \alpha_{married_[j]}
# use student_t priors for intercept
# \alpha_j ~ student_t()

#fixed effects

options(mc.cores = parallel::detectCores() - 1)
my_chains <- ifelse(parallel::detectCores() == 1,
                    2,
                    parallel::detectCores() - 1)

# There's some problems w/ parallel computing in R 4.0, an RStudio problem
## WORKAROUND: https://github.com/rstudio/rstudio/issues/6692
## Revert to 'sequential' setup of PSOCK cluster in RStudio Console on macOS and R 4.0.0
if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
    Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}
# res <- mrp(my_formula,
#            surv = aes,
#            ps = au_psw,
#            aux = aux,
#            const = "division",
#            type = "continuous",
#            chains = my_chains,
#            iter = 500,
#            warmup = 250,
#            weight.var = "pop_weight",
#            adapt_delta = 0.99,
#            seed = 181518)

fit_ideo <- stan_glmer(
    ideology ~  (1 | gender) +  (1 | age) + (1 | married) +  (1 | education) + 
        (1 | division) + citizen,
    family = gaussian,
    prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = TRUE),
    prior = rstanarm::student_t(5, 0, 2.5, autoscale = TRUE),
    adapt_delta = 0.99,
    #QR = TRUE,
    data = aes_aux,
    seed = 20200603
)
#tidybayes::get_variables(fit_ideo)



summary(fit_ideo, 
        pars = c("(Intercept)", "sigma", "Sigma[division:(Intercept),(Intercept)]"),
        probs = c(0.025, 0.975),
        digits = 2)

###### ------ it's theoretically possible to do MRP with ordinal outcome but difficult...
# fit_pid <- stan_polr(
#     pid ~  gender + age + married +  education + division,
#     method = "logistic",
#     #location of R^2, proportion of variance in the outcome attributable to the predictors
#     # Beta prior
#     prior = R2(0.25, "mean"),
#     adapt_delta = 0.99,
#     data = aes,
#     seed = 20200603
# )
# print(fit_pid)
# ?prior_summary.stanreg
### try brms package instead
# library(brms)
# fit_pid_brms <- brm(
#     formula = pid ~ 1 + gender + age + married +  education + division,
#     data = aes,
#     family = cumulative("logit")
# )



### ------- model checking --------- ###

## Posterior predictive checking (PPC)
# ppc doesn't seem to capture the multimodality of
# the data very well...
pp_check(fit_ideo)

## prior to posterior
posterior_vs_prior(fit_ideo, group_by_parameter = TRUE, pars=c("(Intercept)"))

### estimate cat preference in the population by accounting for differences 
#between our sample and the population. We use the posterior_linpred() function to 
#obtain posterior estimates for ideoo given the proportion of people in the population 
#in each level of the factors included in the model.
posterior_prob <- posterior_linpred(fit_ideo, 
                                    transform = TRUE, 
                                    newdata = psw_aux)
poststrat_prob <- posterior_prob %*% psw_aux$freq / sum(psw_aux$freq)
model_popn_pref <- c(mean = mean(poststrat_prob), sd = sd(poststrat_prob))
round(model_popn_pref, 3)

#We can compare this to the estimate we would have made if we had just used the sample:
sample_popn_pref <- mean(aes$ideology)
round(sample_popn_pref, 3)


### ------- ideology by state--------- ###

division_df <- data.frame(
    id = 1:150,
    model_div_sd = rep(-1, 150),
    #model_div_cil = rep(-1, 150),
    #model_div_ciu = rep(-1, 150),
    model_div_pref = rep(-1, 150),
    sample_div_pref = rep(-1, 150),
    N = rep(-1, 150)
)

for(i in 1:length(levels(as.factor(psw_aux$id)))) {
    poststrat_div <- psw_aux[psw_aux$id == i, ]
    #draw from the posterior of predictors
    posterior_prob_div <- posterior_linpred(
        fit_ideo,
        transform = TRUE,
        draws = 1000,
        newdata = as.data.frame(poststrat_div)
    )
    poststrat_prob_div <- (posterior_prob_div %*% poststrat_div$freq) / sum(poststrat_div$freq)
    #This is the estimate for popn in state:
    division_df$model_div_pref[i] <- round(mean(poststrat_prob_div), 4)
    division_df$model_div_sd[i] <- round(sd(poststrat_prob_div), 4)
    #This is the estimate for sample
    division_df$sample_div_pref[i] <- round(mean(aes_aux$ideology[aes_aux$id == i], na.rm = TRUE), 4)
    division_df$N[i] <- length(aes_aux$ideology[aes_aux$id == i])
}
mrp_df <- merge(division_df, 
                     select(aes_aux, c(division, id)) %>% distinct(),
                     by = "id")

write_csv(mrp_df, "~/Dropbox/Thesis/inc_adv/raw_data/surveys/mrp.csv")

### ------- vizualise --------- ###
library(ggplot2)

ideo <- read.csv("~/Dropbox/Thesis/inc_adv/raw_data/surveys/mrp.csv")


nat_map16 <- nat_map_download(2016)
nat_data16 <- nat_data_download(2016)

ggplot(aes(map_id=id), data=nat_data16) +
    geom_map(aes(fill=state), map=nat_map16, col = "grey50") +
    expand_limits(x=nat_map16$long, y=nat_map16$lat) + 
    theme_map() + coord_equal()

