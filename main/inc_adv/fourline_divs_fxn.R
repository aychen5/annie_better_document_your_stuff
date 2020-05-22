library(tidyverse)
library(stringr)
##### a function to detect which divisions have "four line separators" #####
# until the two-candidate vote share results are presented.

fourline_divs_fxn <- function (data = lines[[1]], state) {
  
  out <- capture.output(for (i in 1:length(data)) {
    if( str_detect(data[i], paste0("[[:upper:]], ", state)) ) {
      index <- 1
      print( data[i] )
      while( !str_detect(data[i - index], ">|===")) {
        print(if_else(str_detect(data[i - index], "---"), 1, 0))
        index <- index + 1
      }
    }})
  
  clean_split <- str_split(out, pattern = "\\[1\\] ") %>% 
    unlist() %>% 
    enframe() %>% 
    filter(value != "")%>% 
    mutate(divs = ifelse(str_detect(value, "\"."), str_extract(value, "[A-Z]+((\\s|-)[A-Z]+)?"), NA)) %>% 
    select(divs, value) %>% 
    # interpolate the divisions
    zoo::na.locf(maxgap = 20, na.rm = FALSE) %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(!is.na(value)) %>% 
    group_by(divs) %>% 
    summarize(num_lines = sum(value))
  
  fourth_line_divs <- clean_split[which(clean_split$num_lines > 2), "divs"]
  # no NAs
  result <- fourth_line_divs$divs[!is.na(fourth_line_divs$divs)]
  
  return(result)
}