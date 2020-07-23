pacman::p_load(tidyverse, rdrobust, rdd, rddensity, gmodels, gridExtra, purrr)

### ze data
clean_path <- "~/Dropbox/Thesis/inc_adv/clean_data"
tpp_data <- read_csv(paste0(clean_path, "/tpp_data.csv")) %>% 
  filter(PartyAb == "ALP")
tcp_data <- read_csv(paste0(clean_path,"/tcp_data.csv"))
fp_data <- read_csv(paste0(clean_path, "/fp_data.csv"))


### ---------------- variables to test --------------- ###

### voter turnout for each election 
turnout <- map(seq(2004, 2016, by = 3), 
               function(x){read_csv(paste0("~/Dropbox/Thesis/inc_adv/raw_data/covariates/turnout_", x, ".csv"), 
                                    skip = 1) %>% 
                   mutate(year = x,
                          DivisionNm = toupper(DivisionNm),
                          turnout = TurnoutPercentage/100)} ) %>% 
  do.call(what = bind_rows)
# add to all data sets
all_tpp <- merge(tpp_data, turnout %>% select(DivisionNm, year, turnout), 
                 by = c("year", "DivisionNm"), all.x = TRUE)
all_tcp <- merge(tcp_data, turnout %>% select(division = DivisionNm, year, turnout), 
                 by = c("year", "division"), all.x = TRUE)
all_fp <- merge(fp_data, turnout %>% select(division = DivisionNm, year, turnout), 
                by = c("year", "division"), all.x = TRUE)
#View(all_tpp)
### party expenditure and donations (1996-2019)
# though, disclosure threshold of $10,000 was implemented in 2005
returns <- read_csv("~/Dropbox/Thesis/inc_adv/raw_data/covariates/election_returns.csv") %>% 
  rename(candidate_name = Name,
         StateAb = State,
         division = `Electorate Name`,
         donations = `Total Donations`,
         nil = `Nil?`,
         expenditure = `Electoral Expenditure`) %>% 
  filter(!str_detect(Event, "by-election|Senate")) %>% 
  mutate(year = as.numeric(str_extract(Event, "[[:digit:]]+")),
         donations = donations/10000,
         expenditure = expenditure/100000,
         last_name = toupper(str_extract(candidate_name, "^[A-Z]+((\\'|\\-)?[a-zA-Z]+)")),
         PartyAb = case_when(str_detect(`Party Name`, "Labor Party") ~ "ALP",
                             str_detect(`Party Name`, "Liberal Party|Liberal National|National Party|Country Liberal|Liberal & Nationals|Liberal/The Nationals") ~ "LNP",
                             ))
# add to all data sets and lag them
all_tpp <- merge(all_tpp, returns %>% select(year, Surname = last_name, expenditure, donations), 
                 by = c("year", "Surname"))
all_tcp <- merge(all_tcp,  returns %>% select(year, Surname = last_name, expenditure, donations),
                 by = c("year", "Surname"))
all_fp <- merge(all_fp, returns %>% select(year, Surname = last_name, expenditure, donations),
                by = c("year", "Surname"))

### previous number of terms (2004-2016)??

### ---------------- imbalance tests for each covar --------------- ###
# tpp data
pre_covariates <- c("open_seat",
                    #"incumbent",
                    #"tcp_t0",
                     "alp_vs_t0",
                     "alp_fp_t0",
                     "lnp_vs_t0",
                     "lnp_fp_t0",
                    #"alp_incumbent",
                     #"lnp_incumbent",
                    "model_div_pref"
                    #"expenditure", # in 10,000s
                    #"donations",
                    #"turnout"
                    )

rdd_imbal_fxn <- function (covar, data){
  out <- rdrobust(y = data[[covar]],
                  x = data$tcp_vote_share,
           #x = data$tcp_vote_share,
           kernel = "triangular",
           #bwselect = 'msetwo',
           c = 0.5, p = 1,
           h = 0.05,
           covs = data$alp_incumbent,
           all = TRUE,
           cluster = data$DivisionNm) 
  coeff <- out$coef[3] # robust
  se <- out$se[3] # robust
  return(c(coeff, se))
}


# control for family-wise error rate using Bonferroni
# Compute the simultaneous confidence intervals,
# all the parameters are covered with 1−𝛼 confidence.
num_tests <- length(pre_covariates)
B_crit5 <- 1-qt(.95/num_tests, nrow(all_tpp) - num_tests)
B_crit10 <- 1-qt(.90/num_tests, nrow(all_tpp) - num_tests)

# put into table
estimates <- map(pre_covariates, 
                 function (x) {
                   rdd_imbal_fxn(covar = x, data = all_tpp)
                 }) %>% 
  do.call(what = rbind) %>% 
  as.data.frame() %>% 
  rename(coef = V1,
         se = V2) %>% 
  mutate(variable = pre_covariates,
         ci_low = ifelse(coef > 0,
                         coef - (qnorm(0.975)*se), 
                         coef + (qnorm(0.975)*se)),
         ci_high =  ifelse(coef > 0,
                           coef + (qnorm(0.975)*se), 
                           coef - (qnorm(0.975)*se)),
         bon_low5 = ifelse(coef > 0,
                          coef - B_crit5*se, 
                          coef + B_crit5*se),
         bon_high5 = ifelse(coef > 0,
                          coef + B_crit5*se, 
                          coef - B_crit5*se),
         bon_low10 = ifelse(coef > 0,
                           coef - B_crit10*se, 
                           coef + B_crit10*se),
         bon_high10 = ifelse(coef > 0,
                            coef + B_crit10*se, 
                            coef - B_crit10*se))

ggplot(data = estimates) +
  geom_errorbarh(aes(xmin = bon_low5, 
                     xmax = bon_high5, 
                     y = variable),
                 height = 0, col = "#38A79F") +
  geom_errorbarh(aes(xmin = bon_low10, 
                     xmax = bon_high10, 
                     y = variable),
                 height = 0.15,
                 col = "#38A79F",
                 size = 1) +
  geom_point(aes(x = coef, y = variable), 
             size = 5, shape = 18,
             col = "#325D80") +
  labs(x = "RD estimate", y = "Pre-treatment Variables", 
       title = "RDD Falsification Tests on Pre-treatment Variables") +
  geom_vline(xintercept = 0, lty = 2) +
  scale_y_discrete(labels = c(
                              "open_seat" = "Open Seat",
                              "turnout" = "Percentage Turnout",
                              "expenditure" = "Party Expenditure",
                              "donations" = "Party Donations",
                              "alp_vs_t0" = "ALP Vote Share\n (t - 1)",
                              "alp_fp_t0" = "ALP First Pref.\n (t - 1)",
                              "lnp_vs_t0" = "Coalition Vote Share\n (t - 1)",
                              "lnp_fp_t0" = "Coalition First Pref.\n (t - 1)",
                              "model_div_pref" = "Division Political\n Ideology"
                              ),
                   ) +
  theme_bw()




### ---------------- state election imbalance tests for each covar --------------- ###
# doesn make sense unless find an effect?



### ---------------- mccrary density --------------- ###
# density_tcp <- DCdensity(runvar = tcp_data$tcp_vote_share,
#                          cutpoint = 0.5, ext.out = TRUE)
# 
# density_tpp <- DCdensity(runvar = tpp_data$alp_vs,
#                          cutpoint = 0.5)


density_tpp <- rdplotdensity(rdd = rddensity( X = tpp_data$tcp_vote_share,
                                              c = 0.5,
                                              bwselect = "each"),
                             X = tpp_data$tcp_vote_share,
                             CIcol = "#325D80",
                             lwd = 1,
                             CIshade = 0.5,
                             CItype = "line",
                             type = "both",
                             pty = 18,
                             pwd = 4,        
                             ylabel = "Density",
                             xlabel = "Two-candidate Preferred Vote Share",
                             pcol = "#325D80",
                             lcol = "#325D80"
                             ) 

density_tcp <- rdplotdensity(rdd = rddensity( X = tcp_data$tcp_vote_share,
                                              c = 0.5,
                                              bwselect = "each"),
                             X = tcp_data$tcp_vote_share,
                             CIcol = "#325D80",
                             lwd = 1,
                             CIshade = 0.5,
                             CItype = "line",
                             type = "both",
                             pty = 18,
                             pwd = 4,
                             ylabel = "Density",
                             xlabel = "Two-candidate Preferred Vote Share",
                             pcol = "#325D80",
                             lcol = "#325D80") 




















