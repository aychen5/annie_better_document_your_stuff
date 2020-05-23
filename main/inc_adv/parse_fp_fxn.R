library(tidyverse)
library(zoo)

##### a function to create clean first preferences data #####

parse_fp_fxn <- function (previous_election, state, data) {
  # previous_election: a string, year of previous election
  # state: a string, one of the Australian states (NSW, QLD, WA, ...)
  # fxn returns a dataframe of fp distributions for each division
  
  # what information do we need?  
  #first preferences 
  fp <- vector("character")
  
  #divisions
  division_names <- vector("character", length = length(data))
  divisionid <- vector("numeric", length = length(data))
  id_count <- 0
  
  # loop through all lines and get the relevant ones
  for (i in 1:length(data)){
    # division names (takes hyphens too)
    if(str_detect(data[i], paste0("[A-Z]+(\\-[A-Z]+)?, ", state))) {
      division_names[i] <- str_extract(string = data[i], pattern = "[A-Z]+((\\-|\\s)[A-Z]+)?")
      divisionid[i] <- id_count + 1
      id_count <- id_count + 1
    } else  {division_names[i] <- NA}
    
    #first preferences 
    if(str_detect(data[i], paste0(previous_election, " two-party majority:"))) { # this string is previous election
      
      # there are divisions that also had by-elections, so have to start at line one later
      start <- if_else(str_detect(data[i + 1], "by-election"), 
                       i + 2,
                       #  2 lines below begins candidates, first preferences
                       i + 1)
      
      # the number of candidates varies between divisions,
      # print all names between the dashes
      while(!str_detect(data[start+1], "---")) {
        fp[start + 1] <- data[start+1]
        start <- start + 1
      }
    } 
  }
  fp_df <- data.frame(# need to expand fp vector because stops 
    # recording after last instance of "1996 two-party.."
    fp = as.character(c(fp, rep(NA, length(division_names)-length(fp)))),
    # match candidate pool with their divisions
    division_name = as.character(lag(division_names, n = 6L)),
    division_id = as.numeric(lag(divisionid, n = 6L)))
  
  # Some divisions are not lagged by 6 rows
  for (i in 1:nrow(fp_df)) {
    if((!is.na(fp_df$division_name[i])) & is.na(fp_df$fp[i]) & (!is.na(fp_df$fp[i+1]))) {
      fp_df$division_name[i+1] <- fp_df$division_name[i]
    } else {NA} 
  }
  
  # dataframe of first preferences
  clean_fp_df <- fp_df %>% 
    filter(!is.na(fp)) %>% 
    # interpolate the divisions
    zoo::na.locf(maxgap = 20, na.rm = FALSE) %>% 
    # add incumbent
    mutate(incumbent = if_else(str_detect(fp, pattern = "\\*"), 1, 0),
           year = 1998,
           state = toupper(state)) %>% 
    # parse party and fp vote share
    separate(fp, into = c("candidate_name", "other"), sep = "   ", extra = "merge") %>% 
    # use regex for the rest
    mutate(candidate_party = str_extract(other, "[A-z]+"),
           vote_count = str_extract(other, "[0-9]+,[0-9]+"),
           fp_vote_share = as.numeric(str_extract(other, "\\s+[0-9]+\\.[0-9]")),
           swing = str_extract(other, "\\s+\\([\\+\\-][0-9]+\\.[0-9]\\)")) %>% 
    select(-other) %>%  #(there are some encoding issues with apostrophes and replace "ST" with "ST-")
    mutate(candidate_name = if_else(str_detect(candidate_name, "[\u0092\u2018\u2019\u201A\u201B\u2032\u2035]"), 
                                    gsub("[\u0092\u2018\u2019\u201A\u201B\u2032\u2035]", "'", candidate_name), candidate_name),
           candidate_name = if_else(str_detect(candidate_name, "St\\s[A-Za-z]+"), 
                                    gsub("St\\s", "ST-", candidate_name), candidate_name)) %>% 
    #remove asterisks first; also tricky b/c some have "Hon" preceding name (make optional), can be "Hon Dr"
    mutate(candidate_name = str_extract(candidate_name, "((Hon|Dr)\\s)?(Dr\\s)?[A-Za-z]+\\s+[A-Za-z]+((\\'|\\-)[A-Za-z]+)?")) %>% 
    #first extract last names, and make all uppercase
    mutate(last_name = toupper(str_extract(candidate_name, "[A-Za-z]+(?:[\\'\\-][a-zA-Z]+)*$"))) 
  
  return(clean_fp_df)
}