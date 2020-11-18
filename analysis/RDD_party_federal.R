pacman::p_load(dplyr, ggplot2, rdrobust, rdd, readr, gtable, stargazer)


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
        
        #if (!is.NULL(covars)) {}
        
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
        
        return(mod)
}


# this is party-level data
#vote share in next election
covars <- c(alp_incumbent)
party_mod1 <- my_rdd_fxn(outcome = alp_vs_t1, 
                         covars = covars,
                         running = alp_vs) # 2 perc point increase
party_mod2 <- my_rdd_fxn(outcome = alp_win_t1,
                         covars = covars,
                         running = alp_vs) # +20% prob of win

# use fp vote share as the outcome 
party_fp_mod <- my_rdd_fxn(outcome = alp_fp_t1, 
                           covars = covars,
                           running = alp_vs)# 4 perc point increase

# LNP instaed of ALP
party_lnp_mod1 <- my_rdd_fxn(outcome = lnp_vs_t1, 
                             covars = covars,
                             running = lnp_vs) # 2 perc point increase
party_lnp_mod2 <- my_rdd_fxn(outcome = lnp_win_t1, 
                             covars = covars,
                             running = lnp_vs)
party_lnp_mod3 <- my_rdd_fxn(outcome = lnp_fp_t1, 
                             covars = covars,
                             running = lnp_vs)# 5 perc point increase

# manual bandwidths
party_bw_mod <- my_rdd_fxn(outcome = alp_fp_t1, running = alp_vs,
                           h = 0.02)# manual bandwidths


# DONUT REGRESSION
donut_data_alp <- tpp_data %>% 
        filter(alp_vs <= 0.455 | alp_vs >= 0.505)

rdrobust(y = donut_data_alp$alp_fp_t1,
         x = donut_data_alp$alp_vs,
         kernel = "triangular",
         bwselect = 'msetwo',
         c = 0.5, p = 1,
         all = TRUE,
         cluster = donut_data_alp$DivisionNm) %>% summary()
rdrobust(y = donut_data_alp$alp_vs_t1,
         x = donut_data_alp$alp_vs,
         kernel = "triangular",
         bwselect = 'msetwo',
         c = 0.5, p = 1,
         all = TRUE,
         cluster = donut_data_alp$DivisionNm) %>% summary()

### --------- table ------------ ###
make_tbl_fxn <- function (vs, win, fp) {
        
        tpp_fed_tbl <- data.frame(Outcome = c("Two-Party Preferences", "Probability of Winning", "First Preferences"),
                                  # these are bias-corrected, robust estimates
                                  RD_estimate = c(vs$coef[3],
                                                  win$coef[3],
                                                  fp$coef[3]),
                                  CI_l = c(vs$ci[3],
                                           win$ci[3],
                                           fp$ci[3]),
                                  CI_u = c(vs$ci[6],
                                           win$ci[6],
                                           fp$ci[6]),
                                  bwth_l = c(vs$bws[1],
                                             win$bws[1],
                                             fp$bws[1]),
                                  bwth_r = c(vs$bws[3],
                                             win$bws[3],
                                             fp$bws[3]),
                                  # effective number of obs 
                                  eff_N_l = c(vs$N_h[1],
                                              win$N_h[1],
                                              fp$N_h[1]),
                                  eff_N_r = c(vs$N_h[2],
                                              win$N_h[2],
                                              fp$N_h[2])
        ) %>% 
                mutate(CIs = paste0("[", round(CI_l, 2), "; ",  round(CI_u, 2), "]"),
                       Eff_N = paste0("(", eff_N_l, ", ", eff_N_r, ")"),
                       Bandwidth = paste0("[", round(bwth_l, 2), "; ", round(bwth_r, 2), "]")) %>% 
                select(Outcome, RD_estimate, CIs, Bandwidth, Eff_N) %>% 
                slice(rep(1:n(), each = 2)) %>% 
                mutate(RD_estimate = ifelse(row_number() %% 2 == 1, round(RD_estimate, 4), CIs),
                       Outcome = ifelse(row_number() %% 2 == 1, Outcome, NA),
                       Bandwidth = ifelse(row_number() %% 2 == 1, Bandwidth, NA),
                       Eff_N = ifelse(row_number() %% 2 == 1, Eff_N, NA)) %>% 
                select(-CIs)
        
        colnames(tpp_fed_tbl) <- c("Outcome", "RD Estimate", "Bandwidths\n [L; R]", "Effective Num. Obs.\n [L; R]")
        stargazer(tpp_fed_tbl, 
                  style = 'apsr',
                  summary = FALSE,
                  notes = "Brackets below RD coefficients represent 95% confidence intervals.")
}

make_tbl_fxn(vs = party_mod1, win = party_mod2, fp = party_fp_mod)
# extra table for lnp instead of alp
make_tbl_fxn(vs = party_lnp_mod1, win = party_lnp_mod2, fp = party_lnp_mod3)


### robustness with other bandwidths

### robustness with other covariates



### --------- plots ------------ ###

### plots ###
my_rdplot_fxn <- function(outcome, running, type,
                          covars,
                          xlab, ylab) {
        
        rd_obj <- rdplot(outcome, 
                         running, 
                         c = 0.5, p = 4,
                         hide = TRUE,
                         covs = covars,
                         #nbins = 20,
                         binselect = "qsmv",  
                         kernel = "triangular")
        
        rd_poly <- rd_obj$vars_poly 
        rd_bins <- rd_obj$vars_bins %>% 
                filter(round(rdplot_mean_x, 3) != 0.50)
        
       p <- ggplot() +
               geom_errorbar(aes(ymin = pmax(rd_bins$rdplot_ci_l, 0), 
                                 ymax = pmin(1, rd_bins$rdplot_ci_r),
                                 x = rd_bins$rdplot_mean_x),
                             color = "#38A79F") +
                geom_point(aes(x = rd_bins$rdplot_mean_x, 
                               y = rd_bins$rdplot_mean_y),
                           size = 2.5, shape = 16, color = "#325D80") +
                geom_line(aes(x = rd_poly$rdplot_x[which(rd_poly$rdplot_x < 0.5)], 
                              y = rd_poly$rdplot_y[which(rd_poly$rdplot_x < 0.5)]),
                          color = "#E0485A", size = 1) +
                geom_line(aes(x = rd_poly$rdplot_x[which(rd_poly$rdplot_x > 0.5)],
                              y = rd_poly$rdplot_y[which(rd_poly$rdplot_x > 0.5)]),
                          color = "#E0485A", size = 1) +
               labs(x = xlab,
                    y = ylab) +
                geom_vline(xintercept = 0.5, size = 1) +
               theme_bw() +
                theme(plot.title = element_text(hjust = 0.5))
       
       if (type %in% c("tcp", "tpp")) {
         p +  scale_y_continuous(limits = c(0.15, .75)) +  
              scale_x_continuous(limits = c(0.35, .65)) 
       } else if (type == "fp") {
               p +  scale_y_continuous(limits = c(0.15, .75)) +  
                       scale_x_continuous(limits = c(0.35, .65)) 
       } else {
          p + scale_y_continuous(limits = c(-0, 1)) +
             scale_x_continuous(limits = c(0.35, .65)) 
       }
}

grid.arrange(
        #fp alp
        my_rdplot_fxn(outcome = tpp_data$alp_fp_t1,
                      running = tpp_data$alp_vs,
                      covars = tpp_data$alp_incumbent,
                      type = "fp",
                      xlab = "ALP Vote Share (t)",
                      ylab = "First Preference Vote Share (t + 1)"),
        #fp lnp
        my_rdplot_fxn(outcome = tpp_data$lnp_fp_t1,
                      running = tpp_data$lnp_vs,
                      covars = tpp_data$lnp_incumbent,
                      type = "fp",
                      xlab = "Coalition Vote Share (t)",
                      ylab = "First Preference Vote Share (t + 1)"),
        #tcp alp
        my_rdplot_fxn(outcome = tpp_data$alp_vs_t1,
                      running = tpp_data$alp_vs,
                      covars = tpp_data$alp_incumbent,
                      type = "tcp",
                      xlab = "ALP Vote Share (t)",
                      ylab = "Two-Party Vote Share (t + 1)"),
        #tcp lnp
        my_rdplot_fxn(outcome = tpp_data$lnp_vs_t1,
                      running = tpp_data$lnp_vs,
                      covars = tpp_data$lnp_incumbent,
                      type = "tcp",
                      xlab = "Coalition Vote Share (t)",
                      ylab = "Two-Party Vote Share (t + 1)"),
        #win alp
        my_rdplot_fxn(outcome = tpp_data$alp_win_t1,
                      running = tpp_data$alp_vs,
                      covars = tpp_data$alp_incumbent,
                      type = "win",
                      xlab = "ALP Vote Share (t)",
                      ylab = "Probability of Winning (t + 1)"),        
        #win lnp
        my_rdplot_fxn(outcome = tpp_data$lnp_win_t1,
                      running = tpp_data$lnp_vs,
                      covars = tpp_data$lnp_incumbent,
                      type = "win",
                      xlab = "Coalition Vote Share (t)",
                      ylab = "Probability of Winning (t + 1)"),
        nrow = 3, ncol = 2
)

### -------------------------- IE over time ------------------------------- ###

pre_1990 <- tpp_data %>% filter(year <= 1990)
post_1990 <- tpp_data %>% filter(year > 1990)

# plott
library(patchwork)
#fp
(my_rdplot_fxn(outcome = pre_1990$alp_fp_t1,
              running = pre_1990$alp_vs,
              covars = pre_1990$alp_incumbent,
              type = "fp",
              xlab = "ALP Vote Share (t)\n PRE-1990",
              ylab = "First Preference Vote Share (t + 1)") +
my_rdplot_fxn(outcome = post_1990$alp_fp_t1,
              running = post_1990$alp_vs,
              covars = post_1990$alp_incumbent,
              type = "fp",
              xlab = "ALP Vote Share (t)\n POST-1990",
              ylab = "First Preference Vote Share (t + 1)") +
#fp
my_rdplot_fxn(outcome = pre_1990$lnp_fp_t1,
              running = pre_1990$lnp_vs,
              covars = pre_1990$lnp_incumbent,
              type = "fp",
              xlab = "Coalition Vote Share (t)\n PRE-1990",
              ylab = "First Preference Vote Share (t + 1)") +
my_rdplot_fxn(outcome = post_1990$lnp_fp_t1,
                      running = post_1990$lnp_vs,
                      covars = post_1990$lnp_incumbent,
                      type = "fp",
                      xlab = "Coalition Vote Share (t)\n POST-1990",
                      ylab = "First Preference Vote Share (t + 1)"))


my_rdd_fxn <- function (outcome, running,clust,
                        covars = NULL, h = NULL) {
        
        mod <- rdrobust(y = outcome,
                        # alp margin of victory at t
                        x = running,
                        kernel = "triangular",
                        bwselect = 'msetwo',
                        h = h,
                        c = 0.5, p = 1,
                        covs = covars,
                        all = TRUE,
                        cluster = clust)
        
        return(mod)
}

##### table
pre_fp_alp <- my_rdd_fxn(outcome = pre_1990$alp_fp_t1, 
           covars = pre_1990$alp_incumbent,
           running = pre_1990$alp_vs,
           clust = pre_1990$DivisionNm) 
post_fp_alp <- my_rdd_fxn(outcome = post_1990$alp_fp_t1, 
                     covars = post_1990$alp_incumbent,
                     running = post_1990$alp_vs,
                     clust = post_1990$DivisionNm) 
pre_fp_lnp <- my_rdd_fxn(outcome = pre_1990$lnp_fp_t1, 
                         covars = pre_1990$lnp_incumbent,
                         running = pre_1990$lnp_vs,
                         clust = pre_1990$DivisionNm) 
post_fp_lnp <- my_rdd_fxn(outcome = post_1990$lnp_fp_t1, 
                          covars = post_1990$lnp_incumbent,
                          running = post_1990$lnp_vs,
                          clust = post_1990$DivisionNm) 

pre_post_df <- data.frame(Outcome = c("Labor First Preferences", "Coalition First Preferences"),
           # these are bias-corrected, robust estimates
           RD_estimate = c(pre_fp_alp$coef[3],
                           pre_fp_lnp$coef[3]),
           CI_l = c(pre_fp_alp$ci[3],
                    pre_fp_lnp$ci[3]),
           CI_u = c(pre_fp_alp$ci[6],
                    pre_fp_lnp$ci[6]),
           RD_estimate_post = c(post_fp_alp$coef[3],
                                post_fp_lnp$coef[3]),
           CI_l_post = c(post_fp_alp$ci[3],
                         post_fp_lnp$ci[3]),
           CI_u_post = c(post_fp_alp$ci[6],
                         post_fp_lnp$ci[6])
           # bwth_l = c(pre_fp_alp$bws[1],
           #            pre_fp_alp$bws[1]),
           # bwth_r = c(pre_fp_alp$bws[3],
           #            pre_fp_lnp$bws[3]),
           # effective number of obs 
           # eff_N_l = c(pre_fp_alp$N_h[1],
           #             pre_fp_lnp$N_h[1]),
           # eff_N_r = c(pre_fp_alp$N_h[2],
           #             pre_fp_lnp$N_h[2])
) %>% 
        mutate(CIs = paste0("[", round(CI_l, 3), "; ",  round(CI_u, 3), "]"),
               #Eff_N = paste0("(", eff_N_l, ", ", eff_N_r, ")"),
               #Bandwidth = paste0("[", round(bwth_l, 2), "; ", round(bwth_r, 2), "]"),
               CIs_post = paste0("[", round(CI_l_post, 3), "; ",  round(CI_u_post, 3), "]")) %>% 
        select(Outcome, RD_estimate, CIs, 
               RD_estimate_post, CIs_post
               #Bandwidth, Eff_N
               ) %>% 
        slice(rep(1:n(), each = 2)) %>% 
        mutate(RD_estimate = ifelse(row_number() %% 2 == 1, round(RD_estimate, 4), CIs),
               Outcome = ifelse(row_number() %% 2 == 1, Outcome, NA),
               RD_estimate_post = ifelse(row_number() %% 2 == 1, round(RD_estimate_post, 4), CIs_post)
               #Bandwidth = ifelse(row_number() %% 2 == 1, Bandwidth, NA),
               #Eff_N = ifelse(row_number() %% 2 == 1, Eff_N, NA)
               ) %>% 
        select(-c(CIs, CIs_post))

colnames(pre_post_df) <- c("Outcome", "Pre-1990 RD Estimate", "Post-1990 RD Estimate"
                           #"Bandwidths\n [L; R]", "Effective Num. Obs.\n [L; R]"
                           )
stargazer(pre_post_df, 
          style = 'apsr',
          summary = FALSE)
