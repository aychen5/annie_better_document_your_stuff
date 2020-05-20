# LOG OF ANOMALIES AND MANUAL FIXES

### --------------- 1998 NSW --------------- ### 

## Cunningham is misscoded in the text file (previous election, 1990 should be 1996)
# add to FP data
cunningham_df <- data.frame(
  division_name = "CUNNINGHAM",
  candidate_name = c(
    "John Curtis",         
    "Margaret Perrott",                       
    "Frank Coluccio",                         
    "Les Robinson",                   
    "Stephen Ivaneza",               
    "Jennifer van der Horn",
    "Hon Stephen MARTIN *",
    "Alan Akgit hurst",                    
    "Robert O'Neill"),                  
  incumbent = c(0, 0, 0, 0, 0, 0, 1, 0, 0),
  candidate_party = c("ON", NA, NA, "Grn", "AD", "Unity", "ALP", "Lib", "CDP"),
  vote_count = as.character(c(5378, NA, NA, 3158, 4108, 550, 37592, 17285, 1755)),
  fp_vote_share = c(7.6, 0.9,  0.7, 4.4, 5.8, 0.8, 52.9, 24.3, 2.5),
  last_name = toupper(c("Curtis", 
                        "Perrott", 
                        "Coluccio", 
                        "Robinson", 
                        "Ivaneza", 
                        "Horn", 
                        "MARTIN", 
                        "Akhurst", 
                        "O'Neill"))
)

# attach to main df
clean_fp_df <- clean_fp_df %>% 
  bind_rows(cunningham_df) %>% 
  arrange(division_name)


## remove redundant New Castle candidates
clean_tcp_df <- filter(clean_tcp_df, !clean_tcp_df$last_name %in% list("GREG", "ALLAN"))

### --------------- 1998 NSW --------------- ### 


