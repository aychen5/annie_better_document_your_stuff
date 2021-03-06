pacman::p_load(tidyverse, rdrobust, rdd, rddensity, gmodels, gridExtra, purrr)

### ze data
clean_path <- "~/Dropbox/Thesis/inc_adv/clean_data"
tpp_data <- read_csv(paste0(clean_path, "/tpp_data.csv")) 
tcp_data <- read_csv(paste0(clean_path,"/tcp_data.csv"))
fp_data <- read_csv(paste0(clean_path, "/fp_data.csv"))

theme_set(
  theme_bw()
)



my_rdd_fxn <- function (outcome, running, data = tpp_data, 
                        covars = NULL, h = NULL, p = 1, c = 0.5) {
  
  attach(data, warn.conflicts = FALSE)
  
  #if (!is.NULL(covars)) {}
  
  mod <- rdrobust(y = outcome,
                  # alp margin of victory at t
                  x = running,
                  kernel = "triangular",
                  bwselect = 'msetwo',
                  h = h,
                  c = c, p = p,
                  covs = covars,
                  all = TRUE,
                  cluster = DivisionNm)
  
  return(mod)
}

### --------- different bandwidths -------------------- ###

diff_bws_fxn<- function(outcome, opt_bw) {
  diff_bws <- map(c(0.01, 0.02, 0.03, 0.04, 0.05, opt_bw), # 0.06 is optimal for this
                  function (h) {my_rdd_fxn(outcome = outcome, 
                                           covars = alp_incumbent,
                                           running = alp_vs,
                                           h = h)})
  RD_estimate <- c()
  CI_l <- CI_u <- c()
  for (i in 1:6) {
    RD_estimate[i] <- diff_bws[[i]]$coef[3]
    CI_l[i] <- diff_bws[[i]]$ci[3]
    CI_u[i] <- diff_bws[[i]]$ci[6]
  }
  out <- data.frame(RD_estimate = RD_estimate, 
                    CI_l = CI_l, CI_u = CI_u,
                    Bandwidth = factor(c(0.01, 0.02, 0.03, 0.04, 0.05, opt_bw)))
  return(out)
}
##### for LABOR
alp_bws <- diff_bws_fxn(outcome = alp_vs_t1,
             opt_bw = 0.06) %>% 
  bind_rows(diff_bws_fxn(outcome = alp_win_t1,
             opt_bw = 0.06)) %>% 
  bind_rows(diff_bws_fxn(outcome = alp_fp_t1,
             opt_bw = 0.06)) %>% 
  mutate(outcome = ifelse(row_number() %in% 1:6, "TPP", 
                          ifelse(row_number() %in% 7:12, "Pr(win)", "First")))
  
ggplot(data = alp_bws) +
  geom_point(aes(x = Bandwidth, y = RD_estimate, col = factor(outcome)),
             position=position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(x = Bandwidth, ymin = CI_l, ymax = CI_u,
                    col = factor(outcome)), width= 0.3,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
  labs(y = "RD Estimate") +
  scale_color_manual(name = "", labels = c("First Preference",
                                           "Pr(win)",
                                           "TPP"),
                     values = c("#38A79F", 
                                "#325D80",
                                "#E0485A")) +
  scale_y_continuous(limits = c(-0.2, 1)) +
  theme(legend.position = 'bottom')

##### for COALITION
lnp_bws <- diff_bws_fxn(outcome = lnp_vs_t1,
                        opt_bw = 0.06) %>% 
  bind_rows(diff_bws_fxn(outcome = lnp_win_t1,
                         opt_bw = 0.06)) %>% 
  bind_rows(diff_bws_fxn(outcome = lnp_fp_t1,
                         opt_bw = 0.06)) %>% 
  mutate(outcome = ifelse(row_number() %in% 1:6, "TPP", 
                          ifelse(row_number() %in% 7:12, "Pr(win)", "First")))

ggplot(data = lnp_bws) +
  geom_point(aes(x = Bandwidth, y = RD_estimate, col = factor(outcome)),
             position=position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(x = Bandwidth, ymin = CI_l, ymax = CI_u,
                    col = factor(outcome)), width= 0.3,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
  labs(y = "RD Estimate") +
  scale_color_manual(name = "", labels = c("First Preference",
                                           "Pr(win)",
                                           "TPP"),
                     values = c("#38A79F", 
                                "#325D80",
                                "#E0485A")) +
  scale_y_continuous(limits = c(-0.2, 1)) +
  theme(legend.position = 'bottom') 

  
### --------- polynomial degrees -------------------- ###
diff_poly_fxn<- function(outcome) {
  diff_poly <- map(1:4, 
                  function (p) {my_rdd_fxn(outcome = outcome, 
                                           covars = alp_incumbent,
                                           running = alp_vs,
                                           p = p)})
  RD_estimate <- c()
  CI_l <- CI_u <- c()
  for (i in 1:4) {
    RD_estimate[i] <- diff_poly[[i]]$coef[3]
    CI_l[i] <- diff_poly[[i]]$ci[3]
    CI_u[i] <- diff_poly[[i]]$ci[6]
  }
  out <- data.frame(RD_estimate = RD_estimate, 
                    CI_l = CI_l, CI_u = CI_u,
                    Polynomial = factor(c(1, 2, 3, 4)))
  return(out)
}
### LABOR
alp_poly <- diff_poly_fxn(outcome = alp_vs_t1) %>% 
  bind_rows(diff_poly_fxn(outcome = alp_win_t1)) %>% 
  bind_rows(diff_poly_fxn(outcome = alp_fp_t1)) %>% 
  mutate(outcome = ifelse(row_number() %in% 1:4, "TPP", 
                          ifelse(row_number() %in% 5:8, "Pr(win)", "First")))

ggplot(data = alp_poly) +
  geom_point(aes(x = Polynomial, y = RD_estimate, col = factor(outcome)),
             position=position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(x = Polynomial, ymin = CI_l, ymax = CI_u,
                    col = factor(outcome)), width= 0.3,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
  labs(y = "RD Estimate") +
  scale_color_manual(name = "", labels = c("First Preference",
                                           "Pr(win)",
                                           "TPP"),
                     values = c("#38A79F", 
                                "#325D80",
                                "#E0485A")) +
  scale_y_continuous(limits = c(-0.2, .8)) +
  theme(legend.position = 'bottom') 

### COALITION
lnp_poly <- diff_poly_fxn(outcome = lnp_vs_t1) %>% 
  bind_rows(diff_poly_fxn(outcome = lnp_win_t1)) %>% 
  bind_rows(diff_poly_fxn(outcome = lnp_fp_t1)) %>% 
  mutate(outcome = ifelse(row_number() %in% 1:4, "TPP", 
                          ifelse(row_number() %in% 5:8, "Pr(win)", "First")))

ggplot(data = lnp_poly) +
  geom_point(aes(x = Polynomial, y = RD_estimate, col = factor(outcome)),
             position=position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(x = Polynomial, ymin = CI_l, ymax = CI_u,
                    col = factor(outcome)), width= 0.3,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
  labs(y = "RD Estimate") +
  scale_color_manual(name = "", labels = c("First Preference",
                                           "Pr(win)",
                                           "TPP"),
                     values = c("#38A79F", 
                                "#325D80",
                                "#E0485A")) +
  scale_y_continuous(limits = c(-0.2, .5)) +
  theme(legend.position = 'bottom') 


### --------- different cutoffs -------------------- ###
diff_threshold_fxn<- function(outcome) {
  thresholds <- seq(from = 0.4, to = 0.6, by = 0.01)
  diff_threshold <- map(thresholds, 
                   function (c) {my_rdd_fxn(outcome = outcome, 
                                            covars = lnp_incumbent,
                                            running = lnp_vs,
                                            c = c)})
  RD_estimate <- c()
  CI_l <- CI_u <- c()
  for (i in 1:length(thresholds)) {
    RD_estimate[i] <- diff_threshold[[i]]$coef[3]
    CI_l[i] <- diff_threshold[[i]]$ci[3]
    CI_u[i] <- diff_threshold[[i]]$ci[6]
  }
  out <- data.frame(RD_estimate = RD_estimate, 
                    CI_l = CI_l, CI_u = CI_u,
                    Thresholds = factor(thresholds))
  return(out)
}

alp_thres <- diff_threshold_fxn(outcome = alp_vs_t1) %>% 
  bind_rows(diff_threshold_fxn(outcome = alp_win_t1)) %>% 
  bind_rows(diff_threshold_fxn(outcome = alp_fp_t1)) %>% 
  mutate(outcome = ifelse(row_number() %in% 1:21, "TPP", 
                          ifelse(row_number() %in% 22:42, "Pr(win)", "First")))

ggplot(data = alp_thres) +
  geom_vline(xintercept = factor(0.5), col = "red", lty = 2) +
  geom_point(aes(x = Thresholds, y = RD_estimate, col = factor(outcome)),
             position=position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(x = Thresholds, ymin = CI_l, ymax = CI_u,
                    col = factor(outcome)), width= 0.4,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
  labs(y = "RD Estimate") +
  scale_color_manual(name = "", labels = c("First Preference",
                                           "Pr(win)",
                                           "TPP"),
                     values = c("#38A79F", 
                                "#325D80",
                                "#E0485A")) +
  scale_y_continuous(limits = c(-0.4, .4)) +
  theme(legend.position = 'bottom') 


lnp_thres <- diff_threshold_fxn(outcome = lnp_vs_t1) %>% 
  bind_rows(diff_threshold_fxn(outcome = lnp_win_t1)) %>% 
  bind_rows(diff_threshold_fxn(outcome = lnp_fp_t1)) %>% 
  mutate(outcome = ifelse(row_number() %in% 1:21, "TPP", 
                          ifelse(row_number() %in% 22:42, "Pr(win)", "First")))

ggplot(data = lnp_thres) +
  geom_vline(xintercept = factor(0.5), col = "red", lty = 2) +
  geom_point(aes(x = Thresholds, y = RD_estimate, col = factor(outcome)),
             position=position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(x = Thresholds, ymin = CI_l, ymax = CI_u,
                    col = factor(outcome)), width= 0.4,
                position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0) +
  labs(y = "RD Estimate") +
  scale_color_manual(name = "", labels = c("First Preference",
                                           "Pr(win)",
                                           "TPP"),
                     values = c("#38A79F", 
                                "#325D80",
                                "#E0485A")) +
  scale_y_continuous(limits = c(-0.5, .4)) +
  theme(legend.position = 'bottom') 




### --------- additional covariates -------------------- ###

# national swing

# number of candidates running in division
candidate_pool <- fp_data %>% 
  group_by(division, year) %>% 
  count(division) %>% 
  rename(number_candidates = n,
         DivisionNm = division)

cand_pool_tpp <- merge(tpp_data, candidate_pool,
      by = c("DivisionNm", "year"))

num_cand_alp <- rdrobust(y = cand_pool_tpp$alp_win_t1,
         # alp margin of victory at t
         x = cand_pool_tpp$alp_vs,
         kernel = "triangular",
         bwselect = 'msetwo',
         c = 0.5, p = 1,
         covs = cand_pool_tpp$number_candidates + cand_pool_tpp$alp_incumbent,
         all = TRUE,
         cluster = cand_pool_tpp$DivisionNm)

num_cand_lnp <-rdrobust(y = cand_pool_tpp$lnp_win_t1,
         # alp margin of victory at t
         x = cand_pool_tpp$lnp_vs,
         kernel = "triangular",
         bwselect = 'msetwo',
         c = 0.5, p = 1,
         covs = cand_pool_tpp$number_candidates + cand_pool_tpp$lnp_incumbent,
         all = TRUE,
         cluster = cand_pool_tpp$DivisionNm) 


# decade dummies
decade_data <- tpp_data %>% 
  mutate(decade = case_when(year <1960 ~ "Fifties",
                            year > 1960 & year <1970 ~ "Sixties",
                            year > 1970 & year <1980 ~ "Seventies",
                            year > 1980 & year <1990 ~ "Eighties",
                            year > 1990 & year <2000 ~ "Nineties",
                            year > 2000 & year <2010 ~ "Twothousands",
                            year > 2010 & year <2020 ~ "Twothousandtens")) %>% 
  fastDummies::dummy_cols(select_columns = c("decade", "StateAb"))

time_space_alp <- rdrobust(y = decade_data$alp_win_t1,
                         # alp margin of victory at t
                         x = decade_data$alp_vs,
                         kernel = "triangular",
                         bwselect = 'msetwo',
                         c = 0.5, p = 1,
                         covs = decade_data$decade_Fifties +
                           decade_data$decade_Sixties +
                           decade_data$decade_Seventies +
                           decade_data$decade_Eighties +
                           decade_data$decade_Nineties +
                           decade_data$decade_Twothousands +
                           decade_data$decade_Twothousandtens +
                           decade_data$StateAb_ACT +
                           decade_data$StateAb_TAS +
                           decade_data$StateAb_NSW +
                           decade_data$StateAb_QLD +
                           decade_data$StateAb_WA +
                           decade_data$StateAb_SA +
                           decade_data$StateAb_VIC +
                           decade_data$StateAb_NT +
                           decade_data$alp_incumbent,
                         all = TRUE,
                         cluster = decade_data$DivisionNm)

time_space_lnp <-rdrobust(y = decade_data$lnp_win_t1,
                        # alp margin of victory at t
                        x = decade_data$lnp_vs,
                        kernel = "triangular",
                        bwselect = 'msetwo',
                        c = 0.5, p = 1,
                        covs = decade_data$decade_Fifties +
                          decade_data$decade_Sixties +
                          decade_data$decade_Seventies +
                          decade_data$decade_Eighties +
                          decade_data$decade_Nineties +
                          decade_data$decade_Twothousands +
                          decade_data$decade_Twothousandtens + 
                          decade_data$StateAb_ACT +
                          decade_data$StateAb_TAS +
                          decade_data$StateAb_NSW +
                          decade_data$StateAb_QLD +
                          decade_data$StateAb_WA +
                          decade_data$StateAb_SA +
                          decade_data$StateAb_VIC +
                          decade_data$StateAb_NT +
                          decade_data$lnp_incumbent,
                        all = TRUE,
                        cluster = decade_data$DivisionNm) 


covariate_tbl <- data.frame(RD_estimate = c(num_cand_alp$coef[3],
                     num_cand_lnp$coef[3],
                     time_space_alp$coef[3],
                     time_space_lnp$coef[3]),
           CI_l = c(num_cand_alp$ci[3],
                    num_cand_lnp$ci[3],
                    time_space_alp$ci[3],
                    time_space_lnp$ci[3]),
           CI_u = c(num_cand_alp$ci[6],
                    num_cand_lnp$ci[6],
                    time_space_alp$ci[6],
                    time_space_lnp$ci[6]),
           bwth_l = c(num_cand_alp$bws[1],
                      num_cand_lnp$bws[1],
                      time_space_alp$bws[1],
                      time_space_lnp$bws[1]),
           bwth_r = c(num_cand_alp$bws[3],
                      num_cand_lnp$bws[3],
                      time_space_alp$bws[3],
                      time_space_lnp$bws[3]),
           # effective number of obs 
           eff_N_l = c(num_cand_alp$N_h[1],
                       num_cand_lnp$N_h[1],
                       time_space_alp$N_h[1],
                       time_space_lnp$N_h[1]),
           eff_N_r = c(num_cand_alp$N_h[2],
                       num_cand_lnp$N_h[2],
                       time_space_alp$N_h[2],
                       time_space_lnp$N_h[2]),
           Outcome = "Pr(Win)",
           Covariate = c(rep("# Candidates in Division", 2),
                         rep("Decade + State Dummies", 2)),
           Party = rep(c("ALP", "Coalition"), 2)
           )%>% 
  mutate(CIs = paste0("[", round(CI_l, 2), "; ",  round(CI_u, 2), "]"),
         Eff_N = paste0("(", eff_N_l, ", ", eff_N_r, ")"),
         Bandwidth = paste0("[", round(bwth_l, 2), "; ", round(bwth_r, 2), "]")) %>% 
  select( Party, RD_estimate, CIs, Bandwidth, Eff_N, Covariate) %>% 
  slice(rep(1:n(), each = 2)) %>% 
  mutate(RD_estimate = ifelse(row_number() %% 2 == 1, round(RD_estimate, 4), CIs),
         Party = ifelse(row_number() %% 2 == 1, Party, NA),
         Bandwidth = ifelse(row_number() %% 2 == 1, Bandwidth, NA),
         Eff_N = ifelse(row_number() %% 2 == 1, Eff_N, NA)) %>% 
  select(-CIs)

colnames(covariate_tbl) <- c("Party", "RD Estimate", "Bandwidths\n [L; R]", 
                             "Effective Num. Obs.\n [L; R]", "Covariate")
stargazer(covariate_tbl, 
          style = 'apsr',
          summary = FALSE,
          notes = "Brackets below RD coefficients represent 95% confidence intervals.")












