pacman::p_load(dplyr, readr, ggplot2, rdrobust, rdd, gmodels, gridExtra)


### ------------ANNIE'S TO-DO's --------------- ###
# - tcp bounds
# - need to merge xxx

###############################################################
######### RDD Analysis (National; Party) ##########
###############################################################
# finally getting to use data painstaking cleaned (=^･ｪ･^=))ﾉ彡☆


### --------------- CANDIDATE-LEVEL ANALYSIS ----------------- ###
tcp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tcp_data.csv")
fp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/fp_data.csv")

same_vars <- intersect(colnames(tcp_data), colnames(fp_data))
tcp_df <- tcp_data %>% 
  select(c(same_vars, open_seat, tcp_vote_share, `division-year`)) %>% 
  group_by(`division-year`)
fp_df <- fp_data %>% 
  select(same_vars)

View(tcp_df)
View(fp_df)
#check if surname in tcp is in fp data
search_name <- function (div, yr) {
  rerun <- c()
  # 1980, 1983, 1984, 1987
  if(yr == 1996) {
    next_election <- 1998
  } else if (yr == 1983) {
    next_election <- 1984
  } else {
    next_election <- yr+3
  }
  
  tcp_rows <- tcp_df %>% filter(`division-year` == paste0(div, "-",yr))
  fp_rows <- fp_df %>% filter(`division-year` == paste0(div, "-", next_election ))
  
  for (i in 1:nrow(tcp_rows)) {
    rerun[i] <- ifelse(tcp_rows$Surname[i] %in% fp_rows$Surname, 1, 0)
  }
  
  return(c(rerun, tcp_rows$Surname, paste0(div, "-",yr)))
}

# vector of length 2 is the two-party candidates, 
# if 0, candidate did not run in last election, 
# if 1, candidate ran in previous election.
#i.e. search_name(div = "ADELAIDE", yr = 2019)

# all division year combinations
candidate_rerun_t1 <- map(
  unique(tcp_df$year),
  function (y){ 
    map(unique(tcp_df$division), 
     function (x){search_name(div = x, yr = y)} 
  )} )

#clean up listå
cand_rerun_t1 <- data.frame(do.call(rbind, unlist(candidate_rerun_t1, recursive=FALSE)),
           stringsAsFactors=FALSE) %>% 
  select(-c(X9, X6, X7, X8)) %>% 
  mutate(X2 = ifelse(str_detect(X2, "-[0-9]+"), 0, X2),
         S1 = na_if(X3, 0),
         S2 = ifelse(str_detect(X4, "-[0-9]+"), NA, X4),
         `division-year` = X5) %>% 
  select(-c(X3, X4, X5)) %>% 
  pivot_longer(-c(`division-year`, X1, X2), names_to = c("Position"), values_to = "Surname") %>% 
  mutate(rerun_t1 = as.numeric(ifelse(Position == "S1", X1, X2)),
         rerun_t1 = ifelse(str_detect(`division-year`, "2019"), NA, rerun_t1)) %>% 
  select(-c(X1, X2, Position))

#merge with all tcp
all_tcp_data <- merge(tcp_data, cand_rerun_t1, 
      by = c("Surname", "division-year")) %>% 
  arrange(division, desc(year)) %>% 
  group_by(division) %>%
  mutate(candidate_fp_t1 = ifelse(rerun_t1 == 1, 
                                  ifelse(div_year_id == 1, ifelse(str_detect(Surname, lag(Surname, n = 2L)), 
                                                                  lag(fp_vote_share, n = 2L),
                                                                  lag(fp_vote_share, n = 1L)), 
                                         ifelse(div_year_id == 2, ifelse(str_detect(Surname, lead(Surname, n = 3L)), 
                                                                         lag(fp_vote_share, n = 3L), 
                                                                         lag(fp_vote_share, n = 2L)), 
                                                NA)),
                                  NA),
         candidate_win_t1 = ifelse(tcp_t1 > 0.5, 1, 0))

### ------------- plots ------------- ###
# percent candidates running in the next election by incumbency
ggplot(subset(all_tcp_data, !is.na(incumbent)& !is.na(rerun_t1))) +
  geom_bar(aes(x = factor(rerun_t1), 
               fill = factor(incumbent)), 
           stat = "count", position = "dodge") +
  labs(x = "", y = "# of candidates who rerun") +
  scale_fill_manual(name = "",
                      labels = c("Not incumbent", "Incumbent"),
                      values = c("#425569", "#DB9694")) +
  scale_x_discrete(labels = c("Does not rerun", "Rerun")) +
  theme_bw() +
  theme(legend.position = 'bottom')

# rdd sample yields substantially different ratios
ggplot(subset(all_tcp_data, 
              ((abs(candidate_margin_t) < 0.05)) & !is.na(incumbent) & !is.na(rerun_t1)) )  +
  geom_bar(aes(x = factor(rerun_t1), 
               fill = factor(incumbent)), 
           stat = "count", position = "dodge") +
  labs(x = "", y = "# of candidates who rerun") +
  scale_fill_manual(name = "",
                    labels = c("Not incumbent", "Incumbent"),
                    values = c("#425569", "#DB9694")) +
  scale_x_discrete(labels = c("Does not rerun", "Rerun")) +
  theme_bw() +
  theme(legend.position = 'bottom')


# Of the 41% in the full sample who are incumbents, 
# 61% choose to rerun.
# Incumbents are 1.6 times more likely to rerun than challengers,
# but barely in rd sample.
# compared to the 3.7 in Canada

CrossTable(all_tcp_data$incumbent, all_tcp_data$rerun_t1,
           prop.t = FALSE, prop.c = TRUE, prop.chisq = FALSE)
CrossTable(subset(all_tcp_data, abs(candidate_margin_t) < 0.05)$incumbent, 
           subset(all_tcp_data, abs(candidate_margin_t) < 0.05)$rerun_t1,
           prop.t = FALSE, prop.c = TRUE, prop.chisq = FALSE)

### --------------- bounds ----------------- ###

bounds_fxn <- function(outcome = all_tcp_data$candidate_fp_t1) {
  # i) average number of bare-winners (within 5% win margin) who rerun 
  close_winners <- subset(all_tcp_data, tcp_vote_share <  0.55 & tcp_vote_share > 0.50)
  prob_rerun <- mean(close_winners$rerun_t1, na.rm = TRUE)
  
  # ii) unconditional RD effect on fps
  uncond <- rdrobust(y = outcome,
                     #y = all_tcp_data$candidate_win_t1,
                     x = all_tcp_data$tcp_vote_share,
                     kernel = "triangular",
                     bwselect = 'msetwo',
                     c = 0.5, p = 1,
                     all = TRUE,
                     cluster = all_tcp_data$division) 
  #summary(uncond)
  unconditional_rd <- uncond$coef[3] # robust
  unconditional_sd <- uncond$se[3]
  
  # iii) RD effect on rerunning (probability of being a complier)
  # marginal winners are more likely to rerun in next race
  first_stage <- rdrobust(y = all_tcp_data$rerun_t1,
                          x = all_tcp_data$tcp_vote_share,
                          kernel = "triangular",
                          bwselect = 'msetwo',
                          c = 0.5, p = 1,
                          all = TRUE,
                          cluster = all_tcp_data$division) 
  firststage_rd <- first_stage$coef[3] # robust
  firststage_sd <- first_stage$se[3] 
  
  # iv) probability that compliers who barely-lost would win in the next election
  # This is unobservable.
  # to create bounds, assume that this loser at t would never win in t+1,
  # and that this loser at t would always win) at t+1
  lwr_bound <- 1
  upp_bound <- 0
  
  # more assumptions: iv) is at as large as the same outcome value for bare-winners
  a1_bound <- mean(close_winners$fp_vote_share, na.rm = TRUE)
  a2_bound <- mean(close_winners$fp_vote_share, na.rm = TRUE)/2
  
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
  
  candidate_estimates <- candidate_estimates %>% 
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
  return(candidate_estimates)
}

# check both outcomes, first preferences and probability of winning
fp_candidate_estimates <- bounds_fxn(outcome = all_tcp_data$candidate_fp_t1)
tcp_candidate_estimates <- bounds_fxn(outcome = all_tcp_data$candidate_win_t1)

## plot the bounds
bounds_plot_fxn <- function(outcome_df, outcome_lab) {
  ggplot(outcome_df, aes(col = factor(grouping))) +
    geom_point(aes(x = coef_est, y = bound_type), 
               size = 2, shape = 1) +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci,  y = bound_type),
                   height = 0.2) +
    geom_vline(xintercept = 0, lty = 2) +
    labs(x = paste0("RD Effect on ", outcome_lab," in the Next Election"), y = "") +
    scale_color_manual(name = "", labels = c("unconditional\n RD estimate",
                                             "upper and lower\n bounds",
                                             "assumptions on\n upper bound"),
                       values = c("#38A79F",
                                  "#E0485A", 
                                  "#325D80")) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.justification = "right",
          aspect.ratio = 1/2)
}

grid.arrange(
  bounds_plot_fxn(outcome_df = fp_candidate_estimates, outcome_lab = "First Preferences") +
    scale_x_continuous(limits = c(-.7, .5)) + 
    scale_y_discrete(labels = c("upper" = "Bare-losers would\n receive no votes", 
                                "unconditional" = "Unconditional\n on rerunning",
                                "lower" = "Bare-losers would\n receive every vote",
                                "a1" = "Bare-losers would receive\n as many votes as\n bare-winners",
                                "a2" = "Bare-losers would receive\n less than half as many votes\n as bare-winners")),
  bounds_plot_fxn(outcome_df = tcp_candidate_estimates, outcome_lab = "Winning") +
    scale_x_continuous(limits = c(-.8, .6)) + 
    scale_y_discrete(labels = c("upper" = "Bare-losers would\n never win", 
                                "unconditional" = "Unconditional\n on rerunning",
                                "lower" = "Bare-losers would\n always win",
                                "a1" = "Bare-losers would win\n as often as bare-winners",
                                "a2" = "Bare-losers would win\n less than half the time\n as bare-winners")) 
  
)







