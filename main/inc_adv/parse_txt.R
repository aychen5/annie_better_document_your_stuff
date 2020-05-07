library(tidyverse)
library(readtext)
library(zoo)

#####################################################
####### Cleaning text data #########
# read all text files
# extract:
# - names of the winners 
# - winning % Of the TCP
# - the first preference distribution
# - the electoral constituency
#####################################################

path <- "~/Documents/GitHub/annie_better_document_your_stuff/other_scripts"

#list.files(path, pattern = ".txt")

data <- readtext(paste0(path, "/out.txt"))

# separate lines by \n
lines <- str_split(data$text, pattern = "\n")
lines

########## How is the data formatted? ##########
# Each text file are the results for one State, 
# divided into constituencies (divisions).
# Divisions begin with the first count, and 
# end with the final two-party distribution.
# Asterisks (*) beside the candidate names 
# represent incumbents.
# Candidates with bolded last names are winners,
# but it seems like this coding isn't consistent.
###############################################

parse_fp_fxn <- function (previous_election, state) {
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
  # division names
  if(str_detect(lines[[1]][i], paste0("[[:upper:]], ", state))) {
    division_names[i] <- str_extract(string = lines[[1]][i], pattern = "\\w+")
    divisionid[i] <- id_count + 1
    id_count <- id_count + 1
  } else  {division_names[i] <- NA}
  
  #first preferences 
    if(str_detect(lines[[1]][i], paste0(previous_election, " two-party majority:"))) { # this string is previous election
      #  2 lines below begins candidates, first preferences
      start <- i+1
      # the number of candidates varies between divisions,
      # print all names between the dashes
      while(!str_detect(lines[[1]][start+1], "---")) {
        fp[start+1] <- lines[[1]][start+1]
        start <- start + 1
      }
    } 
}
  fp_df <- data.frame(# need to expand fp vector because stops 
    # recording after last instance of "1996 two-party.."
    fp = c(fp, rep(NA, 4300-4245)),
    # match candidate pool with their divisions
    DivisionNm = lag(division_names, n = 6L),
    division_ID = lag(divisionid, n = 6L))
  
  return(fp_df)
}

fp_df <- parse_fp_fxn(previous_election = "1996", 
                      state = "NSW")

# dataframe of first preferences
clean_fp_df <- fp_df %>% 
  filter(!is.na(fp)) %>% 
  # interpolate the divisions
  zoo::na.locf(maxgap = 20, na.rm = FALSE) %>% 
  # add incumbent
  mutate(incumbent = if_else(str_detect(fp, pattern = "\\*"), 1, 0),
         year = 1998,
         StateAb = "NSW")
 # parse party and fp vote share
View(clean_fp_df)

########### ------------ TCP ------------ #################
parse_tcp_fxn <- function (fourth_line_divs, state) {
  # fourth_line_divs: a vector, division names (in capital letters)
  # state: a string, one of the Australian states (NSW, QLD, WA, ...)
  
  #the final count
  tcp <- list("character")
  
  for (i in 1:length(lines[[1]])){
    ### final count (tcp)
    # three "---" separators above is the tpp distribution
    if (str_detect(lines[[1]][i], paste0("[[:upper:]], ", state))) {
      #function to count the number of separators
      detect_lines_fxn <- function (separator_number, 
                                    data = lines[[1]],
                                    index = i) {
        detect_lines <- c()
        for (j in 1:15) {
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
      
      ### some districts where the tcp is after fourth separator
      # need to manually inspect these
      fourth_line <- paste0(fourth_line_divs, 
                            collapse = "|")
      
      if (str_detect(lines[[1]][i], fourth_line)) {
        tcp[[i]] <- detect_lines_fxn(separator_number = 4) 
      } else {
        tcp[[i]] <- detect_lines_fxn(separator_number = 3) 
      }

    }
  }
  ### don't forget to add the last division
  detect_lines <- c()
  last_lines <- tail(lines[[1]], n = 10)
  
  for (k in 1:(length(last_lines)-1)) {  
    # same trick as before
    detect_lines[k] <- str_detect(last_lines[10-k], "---")
    line_separator_id <- which(detect_lines)[2]
  }
  
  # add last division to rest of list
  tcp <- c(unlist(tcp), 
           last_lines[line_separator_id],
           last_lines[line_separator_id+1])
  
  ### still need to align the divisions 
  return(tcp)
}


tcp_df <- data.frame(tcp = parse_tcp_fxn(fourth_line_divs = c("KINGSFORD-SMITH","HUNTER","NEW ENGLAND"),
                        state = "NSW")) %>% 
  filter(!str_detect(tcp, "character"))

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
  

View(clean_tcp_df)


### fix automated mistakes and data anomallies ###

# Lindsay division candidates were not captured
# Need some way to capture which divisions are "fourth_line_divisions"
# tcp dataframe still needs to have party of candidate


