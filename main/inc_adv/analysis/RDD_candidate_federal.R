pacman::p_load(dplyr, ggplot2, rdrobust, rdd)


### ------------ANNIE'S TO-DO's --------------- ###
# - tcp bounds

###############################################################
######### RDD Analysis (National; Party) ##########
###############################################################
# finally getting to use data painstaking cleansed (=^･ｪ･^=))ﾉ彡☆


### --------------- CANDIDATE-LEVEL ANALYSIS ----------------- ###
tcp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/tcp_data.csv")
glimpse(tcp_data)

rdrobust(y = outcome,
         x = candidate_margin_t,
         kernel = "triangular",
         bwselect = 'msetwo',
         c = 0, p = 1,
         all = TRUE,
         cluster = division)

# number of candidates