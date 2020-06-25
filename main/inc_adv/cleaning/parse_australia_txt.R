library(tidyverse)
library(readtext)
library(zoo)


#--------------------- Annie's TO-DOs: --------------------- #
#- 1998....1949
#- add checks

#####################################################
####### Cleaning text data #########
# read all text files
# extract:
# - names of the winners 
# - TCP vote shares
# - the first preference distribution 
# - the electoral constituency
#####################################################

read_file_fxn <- function (year, state) {
  
  # directory containing files
  path <- "~/Dropbox/Thesis/inc_adv/raw_data/federal/lower"
  year_dir <- paste0("/", year)
  file_name <- paste0("/fp_", tolower(state), "_", year, ".txt")
  
  data <- readtext(paste0(path, year_dir, file_name)) 
  # separate lines by \n
  lines <- str_split(data$text, pattern = "\n")
  
  # remove all lines with "exhausted"
  detect_anomaly <- list()
  for (i in 1:length(lines[[1]])){
    detect_anomaly[[i]] <- ifelse(str_detect(lines[[1]][i], "exhausted"), 1, 0)
  }
  
  out <- data.frame(ind = 1:length( lines[[1]]),
                    exhaust = unlist(detect_anomaly),
                    text = lines[[1]]) %>% 
    filter(exhaust == 0 ) %>% 
    select(text) 
  lines[[1]] <- as.character(out$text)
  
  return(lines)
} 
save_file_fxn <- function(year, state) {
  
  save_path <- "~/Dropbox/Thesis/inc_adv/clean_data"
  year_dir <- paste0("/", year)
  file_name_tcp <- paste0("/tcp-", state, "-", year, ".csv")
  file_name_fp <- paste0("/fp-", state, "-", year, ".csv")
  
  write.csv(clean_tcp_df, paste0(save_path, year_dir, file_name_tcp))
  write.csv(clean_fp_df, paste0(save_path, year_dir, file_name_fp))
  
}

#list.files(path, pattern = ".txt")
#states <- c("nsw", "vic", "qld", "wa", "sa", "tas", "nt", "act")
#1951, 1955, 1958, 961, 1963, 1966, 1972, 1975, 1977, 1980, 1983, 1984, 1987...
#
elect_year <- 1977
prev_year <- "1975"
state <- "NT"

# read in all states in a year
#map(states, function (x) read_file_fxn(year = elect_year, state = x))

lines <- read_file_fxn(year = elect_year, state = state)


########## How is the raw data formatted? ##########
# Each text file are the results for one State, 
# divided into constituencies (divisions).
# Divisions begin with the first count, and 
# end with the final two-party distribution.
# Asterisks (*) beside the candidate names 
# represent incumbents.
# Candidates with bolded last names are winners,
# but it seems like this coding isn't consistent.
###################################################


########### ------------------------ FP ------------------------ #################


parse_fp_fxn <- function (previous_election, state, elect_year) {
  # previous_election: a string, year of previous election
  # state: a string, one of the Australian states (NSW, QLD, WA, ...)
  # fxn returns a dataframe of fp distributions for each division

  # what information do we need?  
  #first preferences 
  fp <- vector("character")
  
  #divisions
  division_names <- vector("character", length = length(lines[[1]]))
  divisionid <- vector("numeric", length = length(lines[[1]]))
  id_count <- 0
  
# loop through all lines and get the relevant ones
for (i in 1:length(lines[[1]])){
  # division names (takes hyphens too)
  if(str_detect(lines[[1]][i], paste0("[A-Z]+((c|\\-|\\s|\\')?[A-Z]+)?, ", state))) {
    division_names[i] <- str_extract(string = lines[[1]][i], pattern = "[A-Z]+((c|\\-|\\s|\\')?[A-Z]+)?")
    divisionid[i] <- id_count + 1
    id_count <- id_count + 1
  } else  {division_names[i] <- NA}
  
  #first preferences 
    if(str_detect(lines[[1]][i], paste0(previous_election, " two-party majority:", "|",
                                        previous_election, " notional two-party majority:", "|",
                                        "New seat"))) { # this string is previous election
      
      # there are divisions that also had by-elections/redistricting, so have to start at line one later
      if(str_detect(lines[[1]][i + 2], "New notional")){
        start <- i + 3 #  3 lines below begins candidates, first preferences
      } else if (str_detect(lines[[1]][i + 1], "by-election|redistribution")) {
        start <- i + 2 #  2 lines below begins candidates, first preferences
      } else {start <- i + 1 }
      
      # the number of candidates varies between divisions,
      # print all names between the dashes
      while(!str_detect(lines[[1]][start+1], "---")) {
        fp[start + 1] <- lines[[1]][start+1]
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
  
  # dataframe of first preferences
  clean_fp_df <- fp_df %>% 
    # interpolate the divisions
    mutate(division_name = zoo::na.locf(division_name, na.rm = FALSE)) %>% 
    # remove extra rows
    filter(!is.na(fp)) %>% 
    # add incumbent
    mutate(incumbent = if_else(str_detect(fp, pattern = "\\*"), 1, 0),
           year = elect_year,
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
    mutate(candidate_name = str_extract(candidate_name, "((Rt|Hon|Dr)\\s)?((Hon|Dr)\\s)?[A-Za-z]+\\s+[A-Za-z]+(?:[\\'|\\-][A-Za-z]+)?")) %>% 
    #first extract last names, and make all uppercase
    mutate(last_name = toupper(str_extract(candidate_name, "[A-Za-z]+(?:[\\'|\\-][a-zA-Z]+)*$"))) 
  
  # # NT has no divisions
  if(state == "NT") {
    clean_fp_df <- clean_fp_df %>% mutate(division_name = rep("NORTHERN TERRITORY", nrow(clean_fp_df)))
  }
  # 
  return(clean_fp_df)
}

clean_fp_df <- parse_fp_fxn(previous_election = prev_year, 
                            state = state, 
                            elect_year = elect_year)


########### ------------------------ TCP ------------------------ #################

parse_tcp_fxn <- function (state, elect_year) {
  
  #the final count
  tcp <- list("character")
  
  # there's only one div for NT
  if(state == "NT") {
    detect_lines <- c()
    last_lines <- tail(lines[[1]], n = 15)
    
    for (k in 1:length(last_lines)) {  
      # same trick as before
      detect_lines[k] <- str_detect(last_lines[16-k], "---")
      line_separator_id <- (length(last_lines)+1) - which(detect_lines)[3]
    }
    
    # add last division to rest of list
    tcp <- c("NORTHERN TERRITORY",
             last_lines[line_separator_id-1],
             last_lines[line_separator_id-2])
    
    tcp_df <- data.frame(tcp = tcp)
  } else {
  
  ### get divisions with extra comments section with custom function ###
  source(here::here("/cleaning/fourline_divs_fxn.R"))
  fourth_line_divs <- fourline_divs_fxn(state = state)
  # state: a string, one of the Australian states (NSW, QLD, WA, ...)
  
  for (i in 1:length(lines[[1]])){
    ### final count (tcp) ###
    # three "---" separators above is the tpp distribution
    if (str_detect(lines[[1]][i], paste0("[[:upper:]], ", state))) {
      #function to count the number of separators
      detect_lines_fxn <- function (separator_number, 
                                    data = lines[[1]],
                                    index = i) {
        detect_lines <- c()
        for (j in 1:18) {
          detect_lines[j] <- str_detect(data[index-j], "---")
        }
        # the two-party count is (often) above the third "---" separator,
        # but might be above the fourth one
        line_separator_id <- index - which(detect_lines)[separator_number]
        
        #two remaining candidates
        return(c(data[index], # the "lagged" division name
                 data[line_separator_id-1],
                 data[line_separator_id-2]))
      }
      
      ### some districts where the tcp is after fourth separator because there's 
      ### an extra section of comments
      fourth_line <- paste0(fourth_line_divs, 
                            collapse = "|")
      # if there are any fourth line separators, skip an extra line
      if (str_detect(lines[[1]][i], fourth_line) & fourth_line != "") {
        tcp[[i]] <- detect_lines_fxn(separator_number = 4) 
      } else {
        tcp[[i]] <- detect_lines_fxn(separator_number = 3) 
      }

    }
  }
  
  ### don't forget to add the last division ###
  detect_lines <- c()
  last_lines <- tail(lines[[1]], n = 15)
  
  for (k in 1:length(last_lines)) {  
    # same trick as before
    detect_lines[k] <- str_detect(last_lines[16-k], "---")
    line_separator_id <- (length(last_lines)+1) - which(detect_lines)[3]
  }
  
  # add last division to rest of list
  tcp <- c(unlist(tcp), 
           last_lines[line_separator_id-1],
           last_lines[line_separator_id-2])
  
  ### need to align the divisions 
  tcp_df <- data.frame(tcp = tcp) %>% 
    filter(!str_detect(tcp, "character"))
  }
  
  divs_df <- tcp_df %>% 
    # all the divisions
    filter(row_number() %in% c(1, seq(from = 2, to = nrow(tcp_df), by = 3))) %>% 
    # remove last row containing extra obs
    slice(1:(max(n())-1)) %>% 
    # parse only division names
    separate(tcp, into = c("division_name", NA),
             sep = ",", extra = "drop") %>% 
    # double for TWO candidates in each division
    slice(rep(1:n(), each = 2))
  
  # join divisions to candidates
  clean_tcp_df <- tcp_df %>% 
    # all the tcp observations
    filter(!row_number() %in% 
             c(1, seq(from = 2, to = nrow(tcp_df), by = 3)[-max(length(seq(from = 2, to = nrow(tcp_df), by = 3)))])) %>% 
    bind_cols(divs_df) %>% 
    mutate(state = toupper(state), 
           year = elect_year) %>% 
    separate(tcp, into = c("candidate_name", "other"), sep = "\\s{2}", extra = "merge") %>% 
    #(there are some encoding issues with apostrophes and replace "ST" with "ST-")
    mutate(candidate_name = if_else(str_detect(candidate_name, "[\u0092\u2018\u2019\u201A\u201B\u2032\u2035]"), 
                                    gsub("[\u0092\u2018\u2019\u201A\u201B\u2032\u2035]", "'", candidate_name), candidate_name),
           candidate_name = if_else(str_detect(candidate_name, "^st|^ST|^St\\s"), 
                                    gsub("^st|^ST|^St\\s", "ST-", candidate_name), candidate_name)) %>% 
    #remove asterisks, may have apostrophe or hyphen in name
    mutate(last_name = toupper(str_extract(candidate_name, "[A-Za-z]+((\\'|\\-)?[a-zA-Z]+)(\\s\\*)?$")),
           last_name = str_extract(last_name, "[A-Za-z]+((\\'|\\-)?[a-zA-Z]+)"),
           tcp_vote_share = str_extract(other, "\\s+[0-9]+\\.[0-9]"))
  
  return(clean_tcp_df)
}

clean_tcp_df <- parse_tcp_fxn(state = state,
                              elect_year = elect_year)


#--------------------- CHECKS --------------------- #
# match candidate names in clean_fp_df to clean_tcp_df to figure out the party
# full names are used in fp and only last names in tcp data

# If any of the fp vote shares are greater than 50%, 
# distribute all others to second place and replace values in tcp df.
fp2tcp_divs <- clean_fp_df[clean_fp_df$fp_vote_share > 50, "division_name"]
#fp2tcp_divs <- fp2tcp_divs[!is.na(fp2tcp_divs)]
fp2tcp_fxn <- function(division) {
  df <- clean_fp_df %>% filter(division_name == division)
  tcp_winner <- df[df$fp_vote_share == max(df$fp_vote_share, na.rm = TRUE), ]
  tcp_winner$tcp_vote_share <- tcp_winner$fp_vote_share
  tcp_second <- df[df$fp_vote_share == sort(df$fp_vote_share, partial = length(df$fp_vote_share)-1)[length(df$fp_vote_share)-1], ]
  tcp_second$tcp_vote_share <- sum(df$fp_vote_share) - tcp_winner$fp_vote_share
  replacements <- bind_rows(tcp_winner, tcp_second) %>% 
    mutate(other = NA)  %>% 
    select(candidate_name, other, division_name, state, year, last_name, tcp_vote_share)
  
  return(replacements)
}

for (div in fp2tcp_divs) {
  # replace in tcp df
  clean_tcp_df[which(clean_tcp_df$division_name == div), ] <- fp2tcp_fxn(division = div)
}

# all names in tcp data must be in fp data! 
tryCatch(
  expr = stopifnot(length(setdiff(clean_tcp_df$last_name, clean_fp_df$last_name))==0),
  # throw error and print missings
  finally = print(setdiff(clean_tcp_df$last_name, clean_fp_df$last_name))
)


View(clean_tcp_df)
View(clean_fp_df) 

#--------------------- MANUAL FIXES/ANOMALIES --------------------- #

# see log_manual_fixes.R script to see all the fixes 
# that are unique to the text file.
#source(here::here("log_manual_fixes.R"))

#--------------------- SAVE CLEAN TCP DATA --------------------- #


# add parties to candidates in tcp data
clean_tcp_df <- clean_tcp_df %>% 
  filter(last_name %in% clean_fp_df$last_name) %>% 
  full_join(clean_fp_df, by = c("division_name", "state", "year", "last_name")) %>% 
  filter(!is.na(tcp_vote_share)) %>% 
  mutate(tcp_vote_share = as.numeric(tcp_vote_share)) %>% 
  select(-c(other, candidate_name.x, swing, division_id))

save_file_fxn(year = elect_year, state = tolower(state))


