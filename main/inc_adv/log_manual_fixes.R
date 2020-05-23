# LOG OF ANOMALIES AND MANUAL FIXES

### --------------- 1998 NSW --------------- ### 

## Cunningham is misscoded in the text file (previous election, 1990 should be 1996)
# add to FP data
if (state == "nsw" & year == 1998) {
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

} else if (state == "vic" & year == 1998) {
### --------------- 1998 NSW --------------- ### 

## MALLEE is a new division in the text file?
mallee_df <- data.frame(
  division_name = "MALLEE",
  candidate_name = c(
    "John FORREST *",         
    "Tom Joyce",                       
    "Bill Croft",                         
    "Lee Cubit",                   
    "Lionel McKenzie",               
    "John Zigouras"),                  
  incumbent = c(1, 0, 0, 0, 0, 0),
  candidate_party = c("NPA", "AD", "ON", NA, NA, "ALP"),
  vote_count = as.character(c(43132, 3440, 9516, 600, 2278, 16471)),
  fp_vote_share = c(57.2, 4.6,12.6, 0.8, 3.0, 21.8),
  last_name = toupper(c("FORREST", 
                        "Joyce", 
                        "Croft", 
                        "Cubit", 
                        "McKenzie", 
                        "Zigouras"))
)

# attach to main df
clean_fp_df <- clean_fp_df %>% 
  bind_rows(mallee_df) %>% 
  arrange(division_name)

}
