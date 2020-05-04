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

# what information do we need?
#divisions
division_names <- vector("character", length = length(lines[[1]]))
divisionid <- vector("numeric", length = length(lines[[1]]))
id_count <- 0

#first preferences 
fp <- vector("character")

#the final count
tcp <- list("character")

# loop through all lines and get the relevant ones
for (i in 1:length(lines[[1]])){
  # division names
  if(str_detect(lines[[1]][i], "[[:upper:]], NSW")) {
    division_names[i] <- str_extract(string = lines[[1]][i], pattern = "\\w+")
    divisionid[i] <- id_count + 1
    id_count <- id_count + 1
  } else  {division_names[i] <- NA}
  
  #first preferences 
    if(str_detect(lines[[1]][i], "1996 two-party majority:")) {
      #  2 lines below begins candidates, first preferences
      start <- i+1
      # the number of candidates varies between divisions,
      # print all names between the dashes
      while(!str_detect(lines[[1]][start+1], "---")) {
        fp[start+1] <- lines[[1]][start+1]
        start <- start + 1
      }
    } 
  
  # final count (tcp)
  # three "---" separators above is the tpp distribution
  if (str_detect(lines[[1]][i], "[[:upper:]], NSW")) {
    detect_lines_fxn <- function (separator_number) {
      detect_lines <- c()
      for (j in 1:15) {
        detect_lines[j] <- str_detect(lines[[1]][i-j], "---")
      }
      # the two-party count is above the third "---" separator
      line_separator_id <- i - which(detect_lines)[separator_number]
      
      #two remaining candidates
      return(c(lines[[1]][i], # the "lagged" division name
               lines[[1]][line_separator_id-1],
               lines[[1]][line_separator_id-2]))
    }
    
    # there are some districts where the tcp is after fourth separator
    # need to manually inspect these
    fourth_line_divs <- paste0(c("KINGSFORD-SMITH",
                                 "HUNTER","NEW ENGLAND"), 
                               collapse = "|")
    
    if (str_detect(tcp[[i]][1], fourth_line_divs)) {
      tcp[[i]] <- detect_lines_fxn(separator_number = 4) 
    } else {
      tcp[[i]] <- detect_lines_fxn(separator_number = 3) 
    }
  }

  # also don't forget to add the last division
}


# dataframe of first preferences
clean_fp_df <- data.frame(# need to expand fp vector because stops 
                       # recording after last instance of "1996 two-party.."
                       fp = c(fp, rep(NA, 4300-4245)),
                       # match candidate pool with their divisions
                       DivisionNm = lag(division_names, n = 6L),
                       division_ID = lag(divisionid, n = 6L)) %>% 
  filter(!is.na(fp)) %>% 
  # interpolate the divisions
  zoo::na.locf(maxgap = 20, na.rm = FALSE) %>% 
  # add incumbent
  mutate(incumbent = if_else(str_detect(fp, pattern = "\\*"), 1, 0),
         year = 1998,
         StateAb = "NSW")
 # parse party and fp vote share
View(clean_fp_df)


# dataframe of two-party winners
unlist(tcp)

clean_tcp_df <- data.frame(tcp = unlist(tcp)) %>% 
  filter(!str_detect(tcp, "character") & !is.na(tcp))




lines[[1]]

### fix automated mistakes and data anomallies ###
# Lindsay division candidates were not captured
# B



