library(tidyverse)
library(zoo)

##### a function to create clean two-party preferred data #####


parse_tcp_fxn <- function (state, data) {
  
  ### get divisions with extra comments section with custom function ###
  source(here::here("fourline_divs_fxn.R"))
  fourth_line_divs <- fourline_divs_fxn(state = state)
  # state: a string, one of the Australian states (NSW, QLD, WA, ...)
  
  #the final count
  tcp <- list("character")
  
  for (i in 1:length(data)){
    ### final count (tcp) ###
    # three "---" separators above is the tpp distribution
    if (str_detect(data[i], paste0("[[:upper:]], ", state))) {
      #function to count the number of separators
      detect_lines_fxn <- function (separator_number, 
                                    data = data,
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
      
      ### some districts where the tcp is after fourth separator because there's 
      ### an extra section of comments
      fourth_line <- paste0(fourth_line_divs, 
                            collapse = "|")
      
      if (str_detect(data[i], fourth_line)) {
        tcp[[i]] <- detect_lines_fxn(separator_number = 4) 
      } else {
        tcp[[i]] <- detect_lines_fxn(separator_number = 3) 
      }
      
    }
  }
  
  ### don't forget to add the last division ###
  detect_lines <- c()
  last_lines <- tail(data, n = 10)
  
  for (k in 1:(length(last_lines)-1)) {  
    # same trick as before
    detect_lines[k] <- str_detect(last_lines[10-k], "---")
    line_separator_id <- 10-which(detect_lines)[3]
  }
  
  # add last division to rest of list
  tcp <- c(unlist(tcp), 
           last_lines[line_separator_id-1],
           last_lines[line_separator_id-2])
  
  ### need to align the divisions 
  tcp_df <- data.frame(tcp = tcp) %>% 
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
    mutate(state = toupper(state), 
           year = 1998) %>% 
    separate(tcp, into = c("candidate_name", "other"), sep = "\\s{2}", extra = "merge") %>% 
    #(there are some encoding issues with apostrophes and replace "ST" with "ST-")
    mutate(candidate_name = if_else(str_detect(candidate_name, "[\u0092\u2018\u2019\u201A\u201B\u2032\u2035]"), 
                                    gsub("[\u0092\u2018\u2019\u201A\u201B\u2032\u2035]", "'", candidate_name), candidate_name),
           candidate_name = if_else(str_detect(candidate_name, "^st|^ST\\s"), 
                                    gsub("^st|^ST\\s", "ST-", candidate_name), candidate_name)) %>% 
    #remove asterisks, may have apostrophe or hyphen in name
    mutate(last_name = toupper(str_extract(candidate_name, "[A-Za-z]+(?:['-][a-zA-Z]+)*")),
           tcp_vote_share = str_extract(other, "\\s+[0-9]+\\.[0-9]"))
  
  return(clean_tcp_df)
}
