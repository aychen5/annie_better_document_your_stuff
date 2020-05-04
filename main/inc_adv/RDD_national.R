pacman::p_load(dplyr, ggplot2, rdrobust, rdd)

###############################################################
######### RDD Analysis (National; Party & Candidate) ##########
###############################################################


# this is party-level data
rdrobust(# margin of victory at t+1
         y = all_data$alp_margin_t1,
         # alp margin of victory at t
         x = all_data$alp_margin_t,
         kernel = "triangular",
         c = 0, p = 1, #h = 0.15, 
         cluster = all_data$division)

rdplot(y = all_data$alp_margin_t1,
       x = all_data$alp_margin_t,
       kernel = "triangular",
       binselect = "qsmv",
       c = 0, p = 4, #h = 0.15, 
       cluster = all_data$division)


qplot(filtered_data$alp_margin_t)
