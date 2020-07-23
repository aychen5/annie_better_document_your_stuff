pacman::p_load(tidyverse, readxl)



### --------------- PARTY-LEVEL ANALYSIS ----------------- ###
state_tpp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tpp_state_data.csv")  

#vote share in next state election
outcomes <- list(state_tpp_data$state_alp_vs_t1, 
              state_tpp_data$state_alp_win_t1, 
              state_tpp_data$state_alp_fp_t1)
state_rdd <- list()
for (y in 1:length(outcomes)){
  state_rdd[[y]] <- rdrobust(y = outcomes[[y]],
           x = state_tpp_data$state_alp_vs,
           kernel = "triangular",
           bwselect = 'msetwo',
           c = 0.5, p = 1,
           all = TRUE,
           cluster = state_tpp_data$district_id)
}
outcomes_lnp <- list(state_tpp_data$state_lnp_vs_t1, 
                 state_tpp_data$state_lnp_win_t1, 
                 state_tpp_data$state_lnp_fp_t1)
state_rdd_lnp <- list()
for (y in 1:length(outcomes_lnp)){
  state_rdd_lnp[[y]] <- rdrobust(y = outcomes_lnp[[y]],
                             x = state_tpp_data$state_lnp_vs,
                             kernel = "triangular",
                             bwselect = 'msetwo',
                             c = 0.5, p = 1,
                             all = TRUE,
                             cluster = state_tpp_data$district_id)
}

###--------------- table ----------------- ###

state_tbl_fxn <- function (data) {
  state_tbl <- data.frame(Outcome = c("Two-Party Preferences", "Probability of Winning", "First Preferences"),
           # these are bias-corrected, robust estimates
           RD_estimate = c(data[[1]]$coef[3],
                           data[[2]]$coef[3],
                           data[[3]]$coef[3]),
           CI_l = c(data[[1]]$ci[3],
                    data[[2]]$ci[3],
                    data[[3]]$ci[3]),
           CI_u = c(data[[1]]$ci[6],
                    data[[2]]$ci[6],
                    data[[3]]$ci[6]),
           bwth_l = c(data[[1]]$bws[1],
                      data[[2]]$bws[1],
                      data[[3]]$bws[1]),
           bwth_r = c(data[[1]]$bws[3],
                      data[[2]]$bws[3],
                      data[[3]]$bws[3]),
           # effective number of obs 
           eff_N_l = c(data[[1]]$N_h[1],
                       data[[2]]$N_h[1],
                       data[[3]]$N_h[1]),
           eff_N_r = c(data[[1]]$N_h[2],
                       data[[2]]$N_h[2],
                       data[[3]]$N_h[2])
) %>% 
  mutate(CIs = paste0("[", round(CI_l, 2), "; ",  round(CI_u, 2), "]"),
         Eff_N = paste0("[", eff_N_l, "; ", eff_N_r, "]"),
         Bandwidth = paste0("(", round(bwth_l, 2), ", ", round(bwth_r, 2), ")")) %>% 
  select(Outcome, RD_estimate, CIs, Bandwidth, Eff_N) %>% 
  slice(rep(1:n(), each = 2)) %>% 
  mutate(RD_estimate = ifelse(row_number() %% 2 == 1, round(RD_estimate, 4), CIs),
         Outcome = ifelse(row_number() %% 2 == 1, Outcome, NA),
         Bandwidth = ifelse(row_number() %% 2 == 1, Bandwidth, NA),
         Eff_N = ifelse(row_number() %% 2 == 1, Eff_N, NA)) %>% 
  select(-CIs)


colnames(state_tbl) <- c("Outcome", "RD Estimate", "Bandwidths\n [L; R]", "Effective Num. Obs.\n [L; R]")
stargazer(state_tbl, 
          style = 'apsr',
          summary = FALSE,
          notes = "Brackets below RD coefficients represent 95% confidence intervals.")
}

state_tbl_fxn(data = state_rdd)
state_tbl_fxn(data = state_rdd_lnp)


###--------------- plots -----------------  ###



### plots ###
my_rdplot_fxn <- function(outcome, running, type,
                          covars, 
                          xlab, ylab) {
  
  rd_obj <- rdplot(outcome, 
                   running, 
                   c = 0.5, p = 3,
                   hide = TRUE,
                   covs = covars,
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
  geom_point(data = rd_bins, aes(x = rdplot_mean_x,
                                 y = rdplot_mean_y),
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
    scale_x_continuous(limits = c(0.35, .65)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  if (type %in% c("tcp", "tpp")) {
    p +  scale_y_continuous(limits = c(0.15, .75)) 
  } else if (type == "fp") {
    p +  scale_y_continuous(limits = c(0.15, .75))
  } else {
    p + scale_y_continuous(limits = c(0, 1)) 
  }
}




grid.arrange(
  # Find function in RDD_party_federal.R
my_rdplot_fxn(state_tpp_data$state_alp_fp_t1,
              state_tpp_data$state_alp_vs,
              covars = state_tpp_data$state_alp_incumbent,
              type = "fp",
              xlab = "ALP Vote Share (t)",
              ylab = "First Preference Vote Share (t + 1)"),
my_rdplot_fxn(state_tpp_data$state_lnp_fp_t1,
              state_tpp_data$state_lnp_vs,
              covars = state_tpp_data$state_alp_incumbent,
              type = "fp",
              xlab = "Coaltion Vote Share (t)",
              ylab = "First Preference Vote Share (t + 1)"),
my_rdplot_fxn(state_tpp_data$state_alp_vs_t1,
              state_tpp_data$state_alp_vs,
              covars = state_tpp_data$state_alp_incumbent,
              type = "tpp",
              xlab = "ALP Vote Share (t)",
              ylab = "Two-Party Vote Share (t + 1)"),
my_rdplot_fxn(state_tpp_data$state_lnp_vs_t1,
              state_tpp_data$state_lnp_vs,
              covars = state_tpp_data$state_lnp_incumbent,
              type = "tpp",
              xlab = "Coaltion Vote Share (t)",
              ylab = "Two-Party Vote Share (t + 1)"),
my_rdplot_fxn(state_tpp_data$state_alp_win_t1,
              state_tpp_data$state_alp_vs,
              covars = state_tpp_data$state_lnp_incumbent,
              type = "win",
              xlab = "ALP Vote Share (t)",
              ylab = "Probability of Winning (t + 1)"),
my_rdplot_fxn(state_tpp_data$state_lnp_win_t1,
              state_tpp_data$state_lnp_vs,
              covars = state_tpp_data$state_lnp_incumbent,
              type = "win",
              xlab = "Coaltion Vote Share (t)",
              ylab = "Probability of Winning (t + 1)"),
nrow = 3
)



### --------------- CANDIDATE-LEVEL ANALYSIS ----------------- ###
state_tcp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tcp_state_data.csv") %>% 
  mutate(`district-year` = paste0(district_name, "-",year)) 
state_fp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/fp_state_data.csv")%>% 
  mutate(`district-year` = paste0(district_name,"-", year))

same_vars <- intersect(colnames(state_tcp_data), colnames(state_fp_data))
state_tcp_df <- state_tcp_data %>% 
  select(c(same_vars, state_tcp_vote, `district-year`)) %>% 
  group_by(`district-year`)
state_fp_df <- state_fp_data  %>%
  select(c(same_vars, `district-year`))


#check if surname in tcp is in fp data
search_name <- function (dis, yr, states) {
  rerun <- c()
  state_tcp_df <- subset(state_tcp_df, state == states)
  state_fp_df <- subset(state_fp_df, state == states)
  
  if(states == "QLD") {
    # 2017, 2015, 2012, 2009, 2004
    next_election <- ifelse(yr %in% c(2009, 2012), yr + 3, 
                            ifelse(yr == 2004, yr + 5,
                                   ifelse(yr == 2015, yr + 2, yr +2)))
  } else {
    next_election <- yr + 4
  }
  
  tcp_rows <- state_tcp_df %>% filter(`district-year` == paste0(dis, "-", yr))
  fp_rows <- state_fp_df %>% filter(`district-year` == paste0(dis, "-", next_election ))
  
  for (i in 1:nrow(tcp_rows)) {
    rerun[i] <- ifelse(tcp_rows$surname[i] %in% fp_rows$surname, 1, 0)
  }
  return(c(rerun, tcp_rows$surname, paste0(dis, "-", yr)))
}

# all division year combinations -- not as straightforward as federal 
nested_fxn <- function (year_vec, state_input) {
  OUT <- map(year_vec,
      function (y) {
        map( unique( subset(state_tcp_df, state == state_input)$district_name ), 
             function (x){search_name(dis = x, yr = y,
                                      states = state_input)})  
      })
  return(OUT)
}

state_rerun_t1 <- unlist(
  list(nested_fxn(year_vec = c(2011, 2015, 2019), state_input = "NSW"),
     nested_fxn(year_vec = c(2014, 2018), state_input = "VIC"),
     nested_fxn(year_vec = c(2004, 2009, 2012, 2015, 2017), state_input = "QLD"),
     nested_fxn(year_vec = c(2014, 2018), state_input = "SA"),
     nested_fxn(year_vec = c(2013, 2017), state_input = "WA"),
     nested_fxn(year_vec = c(2012, 2016), state_input = "NT")),
     recursive = FALSE)

#clean up list
state_cand_rerun_t1 <- data.frame(do.call(rbind, unlist(state_rerun_t1, recursive=FALSE)),
                            stringsAsFactors=FALSE) %>% 
  select(-c(X9, X6, X7, X8)) %>% 
  mutate(X2 = ifelse(str_detect(X2, "-[0-9]+"), NA, X2),
         S1 = na_if(X3, 0),
         S2 = ifelse(str_detect(X4, "-[0-9]+"), NA, X4),
         `district-year` =  na_if(X5, 0)) %>% 
  select(-c(X3, X4, X5)) %>% 
  pivot_longer(-c(`district-year`, X1, X2), names_to = c("position"), values_to = "surname") %>% 
  mutate(state_rerun_t1 = as.numeric(ifelse(position == "S1", X1, X2)),
         state_rerun_t1 = ifelse(str_detect(`district-year`, "2019"), NA, state_rerun_t1)) %>% 
  select(-c(X1, X2, position))

#merge with all tcp
all_state_tcp <- merge(state_tcp_data, state_cand_rerun_t1, 
                      by = c("surname", "district-year")) %>% 
  arrange(district_name, desc(year)) %>% 
  group_by(district_name) %>%
  mutate(state_cand_fp_t1 = ifelse(state_rerun_t1 == 1, 
                                  ifelse(div_year_id == 1, ifelse(str_detect(surname, lag(surname, n = 2L)), 
                                                                  lag(state_fp_vote, n = 2L),
                                                                  lag(state_fp_vote, n = 1L)), 
                                         ifelse(div_year_id == 2, ifelse(str_detect(surname, lead(surname, n = 3L)), 
                                                                         lag(state_fp_vote, n = 3L), 
                                                                         lag(state_fp_vote, n = 2L)), 
                                                NA)),
                                  NA),
         state_candidate_win_t1 = ifelse(state_tcp_t1 > 0.5, 1, 0),
         # replace final years with nas
         state_rerun_t1 = case_when( (state == "NSW" & year == 2019) ~ 3,
                                     (state == "VIC" & year == 2018) ~ 3,
                                     (state == "QLD" & year == 2017) ~ 3,
                                     (state == "WA" & year == 2017) ~ 3,
                                     (state == "SA" & year == 2018) ~ 3,
                                     (state == "NT" & year == 2016) ~ 3,
                                     TRUE ~ state_rerun_t1),
         state_rerun_t1 = na_if(state_rerun_t1, 3))


View(all_state_tcp)

### ------------- plots ------------- ###
ggplot(subset(all_state_tcp, !is.na(incumbent)& !is.na(state_rerun_t1))) +
  geom_bar(aes(x = factor(state_rerun_t1), 
               fill = factor(incumbent)), 
           stat = "count", position = "dodge") +
  labs(x = "", y = "# of candidates who rerun") +
  scale_fill_manual(name = "",
                    labels = c("Not incumbent", "Incumbent"),
                    values = c("#425569", "#DB9694")) +
  scale_x_discrete(labels = c("Does not rerun", "Rerun")) +
  theme_bw() +
  theme(legend.position = 'bottom')


CrossTable(all_state_tcp$incumbent, all_state_tcp$state_rerun_t1,
           prop.t = FALSE, prop.c = TRUE, prop.chisq = FALSE)




### --------------- bounds ----------------- ###

  # i) average number of bare-winners (within 5% win margin) who rerun 
  close_winners <- subset(all_state_tcp, state_tcp_vote <  0.55 & state_tcp_vote > 0.50)
  prob_rerun <- mean(close_winners$state_rerun_t1, na.rm = TRUE)
  
  # ii) unconditional RD effect on fps
  uncond <- rdrobust(#y = all_state_tcp$state_cand_fp_t1,
                     #y = all_state_tcp$state_tcp_t1,
                     y = all_state_tcp$state_candidate_win_t1,
                     x = all_state_tcp$state_tcp_vote,
                     kernel = "triangular",
                     bwselect = 'msetwo',
                     c = 0.5, p = 1,
                     all = TRUE,
                     cluster = all_state_tcp$district_name) 
  #summary(uncond)
  unconditional_rd <- uncond$coef[3] # robust
  unconditional_sd <- uncond$se[3]
  
  # iii) RD effect on rerunning (probability of being a complier)
  # marginal winners are more likely to rerun in next race
  first_stage <- rdrobust(y = all_state_tcp$state_rerun_t1,
                          x = all_state_tcp$state_tcp_vote,
                          kernel = "triangular",
                          bwselect = 'msetwo',
                          c = 0.5, p = 1,
                          all = TRUE,
                          cluster = all_state_tcp$district_name) 
  firststage_rd <- first_stage$coef[3] # robust
  firststage_sd <- first_stage$se[3] 
  
  # iv) probability that compliers who barely-lost would win in the next election
  # This is unobservable.
  # to create bounds, assume that this loser at t would never win in t+1,
  # and that this loser at t would always win) at t+1
  lwr_bound <- 1
  upp_bound <- 0
  
  # more assumptions: iv) is at as large as the same outcome value for bare-winners

    # a1_bound <- mean(close_winners$state_fp_vote, na.rm = TRUE)
    # a2_bound <- mean(close_winners$state_fp_vote, na.rm = TRUE)/2

    # a1_bound <- mean(close_winners$state_tcp_t1, na.rm = TRUE)
    # a2_bound <- mean(close_winners$state_tcp_t1, na.rm = TRUE)/2

    a1_bound <- mean(close_winners$state_candidate_win_t1, na.rm = TRUE)
    a2_bound <- mean(close_winners$state_candidate_win_t1, na.rm = TRUE)/2


    # 
  # put together, effect on fp
  candidate_estimates <- data.frame(
    coef_est = c(
      unconditional_rd,
      # if bare-loser would always win in t+1
      (unconditional_rd - (firststage_rd*lwr_bound))/prob_rerun,
      # if bare-loser would never win in t+1
      (unconditional_rd - (firststage_rd*upp_bound))/prob_rerun,
      (unconditional_rd - (firststage_rd*a1_bound))/prob_rerun, 
      (unconditional_rd - (firststage_rd*a2_bound))/prob_rerun
    ),
    se_est = c(
      unconditional_sd,
      # if bare-loser would always win in t+1
      (unconditional_sd - (firststage_sd*lwr_bound))/prob_rerun,
      # if bare-loser would never win in t+1
      (unconditional_sd - (firststage_sd*upp_bound))/prob_rerun,
      (unconditional_sd - (firststage_sd*a1_bound))/prob_rerun, 
      (unconditional_sd - (firststage_sd*a2_bound))/prob_rerun
    ),
    bound_type = c(
      "unconditional", "lower", "upper", "a1", "a2"
    ),
    grouping = c(
      1, 2, 2, 3, 3
    )
  )
  
  # 95% confidence intervals
  ci_fxn <- function(bound_type) {
    crit_val <- qnorm(0.975, 0, 1)
    if (bound_type %in% c("upper", "unconditional")){
      lw <- (candidate_estimates[candidate_estimates$bound_type == bound_type, "coef_est"]) - crit_val*(candidate_estimates[candidate_estimates$bound_type == bound_type, "se_est"])
      up <- (candidate_estimates[candidate_estimates$bound_type == bound_type, "coef_est"]) + crit_val*(candidate_estimates[candidate_estimates$bound_type == bound_type, "se_est"])
    } else {
      lw <- (candidate_estimates[candidate_estimates$bound_type == bound_type, "coef_est"]) + crit_val*(candidate_estimates[candidate_estimates$bound_type == bound_type, "se_est"])
      up <- (candidate_estimates[candidate_estimates$bound_type == bound_type, "coef_est"]) - crit_val*(candidate_estimates[candidate_estimates$bound_type == bound_type, "se_est"])
    }
    return(c(lw, up))
  }
  
  ### --------------- plot ----------------- ###
  candidate_estimates_win <- candidate_estimates %>% 
    mutate(lower_ci = c(ci_fxn(bound_type = "unconditional")[1],
                        ci_fxn(bound_type = "lower")[1],
                        ci_fxn(bound_type = "upper")[1],
                        ci_fxn(bound_type = "a1")[1],
                        ci_fxn(bound_type = "a2")[1]),
           upper_ci = c(ci_fxn(bound_type = "unconditional")[2],
                        ci_fxn(bound_type = "lower")[2],
                        ci_fxn(bound_type = "upper")[2],
                        ci_fxn(bound_type = "a1")[2],
                        ci_fxn(bound_type = "a2")[2]))


# check both outcomes, first preferences and probability of winning
  bounds_plot_fxn <- function(outcome_df, outcome_lab) {
    ggplot(outcome_df, aes(col = factor(grouping))) +
      geom_point(aes(x = coef_est, y = bound_type), 
                 size = 2, shape = 1) +
      geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci,  y = bound_type),
                     height = 0.2) +
      geom_vline(xintercept = 0, lty = 2) +
      labs(x = paste0("RD Effect on ", outcome_lab," in the Next State Election"), y = "") +
      scale_color_manual(name = "", labels = c("unconditional\n RD estimate",
                                               "conditional\n bounds",
                                               "assumptions on\n upper bound"),
                         values = c("#38A79F",
                                    "#E0485A", 
                                    "#325D80")) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.justification = "right",
            aspect.ratio = 1/2)
  }
  
  grid.arrange(
  bounds_plot_fxn(outcome_df = candidate_estimates_fp, outcome_lab = "First Preferences")+
    scale_x_continuous(limits = c(-.7, .5)) + 
    scale_y_discrete(labels = c("upper" = "Bare-losers would\n receive no votes", 
                                "unconditional" = "Unconditional\n on rerunning",
                                "lower" = "Bare-losers would\n receive every vote",
                                "a1" = "Bare-losers would receive\n as many votes as\n bare-winners",
                                "a2" = "Bare-losers would receive\n less than half as many votes\n as bare-winners")),
  bounds_plot_fxn(outcome_df = candidate_estimates_tcp, outcome_lab = "Two-candidate Preferences")+
    scale_x_continuous(limits = c(-.7, .5)) + 
    scale_y_discrete(labels = c("upper" = "Bare-losers would\n receive no votes", # this is impossible
                                "unconditional" = "Unconditional\n on rerunning",
                                "lower" = "Bare-losers would\n receive every vote",  # so is this
                                "a1" = "Bare-losers would receive\n as many votes as\n bare-winners",
                                "a2" = "Bare-losers would receive\n less than half as many votes\n as bare-winners")),
  bounds_plot_fxn(outcome_df = candidate_estimates_win, outcome_lab = "Winning")+
    scale_x_continuous(limits = c(-.7, 1)) + 
    scale_y_discrete(labels = c("upper" = "Bare-losers would\n never win", 
                                "unconditional" = "Unconditional\n on rerunning",
                                "lower" = "Bare-losers would\n always win",
                                "a1" = "Bare-losers would win\n as often as bare-winners",
                                "a2" = "Bare-losers would win\n half as often as bare-winners"))
  )
  



  
