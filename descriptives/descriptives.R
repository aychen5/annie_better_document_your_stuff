library(tidyverse)
library(psych)
library(xtable)

#####################################################
##### Descriptive statistics [input clean data] #####
#####################################################

tpp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tpp_data.csv")
tcp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tcp_data.csv")
fp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/fp_data.csv")
state_tpp <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tpp_state_data.csv") %>% 
  filter(partyab == "ALP")
state_tcp <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tcp_state_data.csv")

theme_set(
  theme_light()
)

#---------- descriptive stats ----------# 
# party-level data
covariates_tpp <- c("fp_vote_share", "alp_vs", "lnp_vs", "alp_win_t", "lnp_win_t",
                "alp_margin_t", "lnp_margin_t", "open_seat", 
                "alp_incumbent", "lnp_incumbent", "model_div_pref", "year")
tpp_tbl <- tpp_data %>% 
  select(covariates_tpp) %>% 
  describe() %>% 
  select(c(n, mean, sd, median, min, max)) %>% 
  xtable(digits = c(0, 0, 2, 2, 2, 2, 2))
row.names(tpp_tbl) <- c("First Preferences",
                        "ALP Vote Share",
                        "Coalition Vote Share",
                        "ALP Win",
                        "Coalition Win",
                        "ALP Victory Margin",
                        "Coalition Victory Margin",
                        "Open Seat",
                        "ALP Incumbent",
                        "Coalition Incumbent",
                        "Division Political Ideology",
                        "Year")


# candidate-level data
covariates_tcp <- c("fp_vote_share", "tcp_vote_share", "candidate_margin_t",
                    "alp_incumbent", "lnp_incumbent", "open_seat", "model_div_pref", "year")
tcp_tbl <- tcp_data %>% 
  select(covariates_tcp) %>% 
  describe() %>% 
  select(c(n, mean, sd, median, min, max)) %>% 
  xtable(digits = c(0, 0, 2, 2, 2, 2, 2))
row.names(tcp_tbl) <- c("First Preferences",
                        "Two-candidate Vote Shares",
                        "Candidate Victory Margin",
                        "ALP Incumbent",
                        "Coalition Incumbent",
                        "Open Seat",
                        "Division Political Ideology",
                        "Year")


# candidate-level FP data
covariates_fp <- c("fp_vote_share", 
                    "incumbent","year")
fp_tbl <- fp_data %>% 
  select(covariates_fp) %>% 
  describe() %>% 
  select(c(n, mean, sd, median, min, max)) %>% 
  xtable(digits = c(0, 0, 2, 2, 2, 2, 2))
row.names(fp_tbl) <- c("First Preferences",
                        "Incumbent",
                        "Year")


###



#---------- state elections ----------# 
#tpp
covariates_state_tpp <- c("state_fp_vote",
                          "state_alp_vs", "state_lnp_vs", 
                          "state_alp_win_t", "state_lnp_win_t",
                    "state_alp_margin_t", 
                    "state_lnp_margin_t", 
                    "state_alp_incumbent", "state_lnp_incumbent", "year")
state_tpp_tbl <- state_tpp %>% 
  select(covariates_state_tpp) %>% 
  describe() %>% 
  select(c(n, mean, sd, median, min, max)) %>% 
  xtable(digits = c(0, 0, 2, 2, 2, 2, 2))
row.names(state_tpp_tbl) <- c("First Preferences",
                        "ALP Vote Share",
                        "Coalition Vote Share",
                        "ALP Win",
                        "Coalition Win",
                        "ALP Victory Margin",
                        "Coalition Victory Margin",
                        "ALP Incumbent",
                        "LNP Incumbent",
                        "Year")

#tcp
covariates_state_tcp <- c("state_fp_vote", "state_tcp_vote", "state_candidate_margin_t",
                    "incumbent", "year")
state_tcp_tbl <- state_tcp %>% 
  select(covariates_state_tcp) %>% 
  describe() %>% 
  select(c(n, mean, sd, median, min, max)) %>% 
  xtable(digits = c(0, 0, 2, 2, 2, 2, 2))
row.names(state_tcp_tbl) <- c("First Preferences",
                        "Two-party Vote Shares",
                        "Candidate Victory Margin",
                        "Incumbent",
                        "Year")



#---------- check incumbency imbalance ----------# 


grid.arrange(
tpp_data %>% 
  filter(alp_incumbent == 1 & alp_vs > .40 & alp_vs < .60) %>% 
ggplot() +
 geom_histogram(aes(alp_vs), boundary = 0.5,
                #binwidth = 0.005,
                fill = "#38A79F", col = "#38A79F",
                alpha = 0.5, binwidth = 0.005) +
  geom_vline(xintercept = 0.5, col = "#E0485A", lwd = 2) +
  scale_y_continuous(limits = c(0, 65)) +
  annotate(geom = "text", x = 0.495, y = 22,
           label = "19", col = "#325D80", size = 5) +
  annotate(geom = "text", x = 0.505, y = 48,
           label = "42", col = "#325D80", size = 5) +
  labs(x = "ALP Incumbent Vote Share (t)", y = "Count"),
tpp_data %>% 
  filter(lnp_incumbent == 1 & lnp_vs > .30 & lnp_vs < .60) %>% 
  ggplot() +
  geom_histogram(aes(lnp_vs), boundary = 0.5,
                 #binwidth = 0.005,
                 fill = "#38A79F", col = "#38A79F",
                 alpha = 0.5, binwidth = 0.005) +
  geom_vline(xintercept = 0.5, col = "#E0485A", lwd = 2) +
  annotate(geom = "text", x = 0.495, y = 20,
           label = "18", col = "#325D80", size = 5) +
  annotate(geom = "text", x = 0.505, y = 57,
           label = "55", col = "#325D80", size = 5) +
  scale_y_continuous(limits = c(0, 65)) +
  labs(x = "Coalition Incumbent Vote Share (t)", y = "Count"),
nrow = 1
)

### all together
tcp_data %>% 
  filter(incumbent == 1 & tcp_vote_share > .3 & tcp_vote_share < .6) %>% 
  ggplot() +
  geom_histogram(aes(tcp_vote_share), boundary = 0.5,
                 binwidth = 0.005,
                 fill = "#38A79F", col = "#38A79F",
                 alpha = 0.5) +
  geom_vline(xintercept = 0.5, col = "#E0485A", lwd = 2) +
  annotate(geom = "text", x = 0.497, y = 31,
           label = "29", col = "#325D80", size = 5) +
  annotate(geom = "text", x = 0.503, y = 91,
           label = "89", col = "#325D80", size = 5) +
  scale_y_continuous(limits = c(0, 110)) +
  labs(x = "Incumbent Vote Share (t)", y = "Count")



#---------- vote share for different parties over time ----------# 

















