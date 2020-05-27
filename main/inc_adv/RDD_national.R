pacman::p_load(dplyr, ggplot2, rdrobust, rdd)


### ------------ANNIE'S TO-DO's --------------- ###
# - tcp bounds
# - 



###############################################################
######### RDD Analysis (National; Party & Candidate) ##########
###############################################################

# finally getting to use data painstaking cleansed (=^･ｪ･^=))ﾉ彡☆
tpp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tpp_data.csv")
tcp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tcp_data.csv")

# this is party-level data
rdrobust(# margin of victory at t+1
         y = tpp_data$alp_margin_t1,
         # alp margin of victory at t
         x = tpp_data$alp_margin_t,
         kernel = "triangular",
         c = 0, p = 1, #h = 0.15, 
         cluster = tpp_data$division)

rdplot(y = tpp_data$alp_margin_t1,
       x = tpp_data$alp_margin_t,
       kernel = "triangular",
       binselect = "qsmv",
       c = 0, p = 4, #h = 0.15, 
       cluster = tpp_data$division)


qplot(tpp_data$alp_margin_t)
