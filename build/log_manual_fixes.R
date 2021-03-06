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
  

  clean_tcp_df[clean_tcp_df$candidate_name == "ST- CLAIR", "last_name"] <- "ST-CLAIR"

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

} else if (state == "qld" & year == 1998) {

### --------------- 1998 QLD --------------- ### 

# redisticting 

# Herbert division: there are two candidates named Lindsay (last name)
clean_fp_df[clean_fp_df$candidate_name == "Hon Ted Lindsay", "last_name"] <- "LINDSAY"
clean_fp_df[clean_fp_df$candidate_name == "Peter Lindsay", "last_name"] <- "LINDSAY"
clean_tcp_df[clean_tcp_df$last_name == "T", "last_name"] <- "LINDSAY"
clean_tcp_df[clean_tcp_df$last_name == "P", "last_name"] <- "LINDSAY"

clean_tcp_df[clean_tcp_df$last_name == "M", "last_name"] <- "SMITH"

} else if (state == "wa" & year == 1998) {
  
  ### --------------- 1998 WA --------------- ### 
  #Hon Warwick Smith
  clean_tcp_df[clean_tcp_df$last_name == "W", "last_name"] <- "SMITH"
  
} else if (state == "nsw|qld|vic" & year == 1996) {
  
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
  
  ## Cunningham is misscoded in the text file (previous election, 1990 should be 1993)

} else if (state == "qld" & year == 1996) {

    # new seat## MALLEE is a new division in the text file?
  longman_df <- data.frame(
    division_name = "LONGMAN",
    candidate_name = c(
      "Jim Dimo",         
      "Pat Bonnice",                       
      "Terence Madden",                         
      "Norman Hegarty",                   
      "Mal Brough",               
      "Tom Bradley",
      "Greg Hollis",
      "Geoffrey Abnett"),                  
    incumbent = c(0, 0, 0, 0, 0, 0, 0, 0),
    candidate_party = c("Grn", "ALP", NA, NA, "Lib", "NPA", "AD", NA),
    vote_count = as.character(c(2145, 21943, 966, 187, 27704, 12611, 5276, 673)),
    fp_vote_share = c(3.0, 30.7, 1.4, 0.3, 39.7, 17.6, 7.4, 0.9),
    last_name = toupper(c("Dimo", 
                          "Bonnice", 
                          "Madden", 
                          "Hegarty", 
                          "Brough", 
                          "Bradley", 
                          "Hollis",
                          "Abnett"))
  )
  
  # attach to main df
  clean_fp_df <- clean_fp_df %>% 
    bind_rows(longman_df) %>% 
    arrange(division_name)
  
} else if (state == "tas" & year == 1996) {
  clean_tcp_df[clean_tcp_df$last_name == "S", "last_name"] <- "SMITH"
  clean_tcp_df[clean_tcp_df$last_name == "W", "last_name"] <- "SMITH"
} else if (state == "nsw" & year == 1993) {
  
  clean_fp_df[clean_fp_df$candidate_name == "Robert de", "last_name"] <- "FEGELY"
  
} else if (state == "vic" & year == 1996) {
  
  clean_tcp_df[clean_tcp_df$candidate_name == "ST-ONE", "last_name"] <- "STONE"
} else if (state == "sa" & year == 1993) {


clean_fp_df[clean_fp_df$candidate_name == "Marco de", "last_name"] <- "CHELLIS"


}  else if (state == "act" & year == 1993) {


clean_tcp_df[clean_tcp_df$last_name == "R", "last_name"] <- "KELLY"

}  else if (state == "nt" & year == 1993) {
  
  
  clean_tcp_df[clean_tcp_df$last_name == "ARTHUR", "last_name"] <- "PALMER"
  clean_tcp_df[clean_tcp_df$last_name == "HON", "last_name"] <- "SNOWDON"
  
} else if (state == "nsw" & year == 1990) {
  
  
  clean_fp_df[clean_fp_df$candidate_name == "David de", "last_name"] <- "MONTFORT"
  clean_fp_df[clean_fp_df$candidate_name == "George der", "last_name"] <- "MATTOSIAN"
}else if (state == "wa" & year == 1990) {
  
  
  clean_fp_df[clean_fp_df$division_name == "O", "division_name"] <- as.factor("O'CONNOR")
} else if (state == "nsw" & year == 1987) {
  
  clean_tcp_df[clean_tcp_df$candidate_name == "Warren Musgrave", "last_name"] <- "MUSGRAVE"
  clean_tcp_df[clean_tcp_df$candidate_name == "David Brock", "last_name"] <- "BROCK"
  clean_tcp_df[clean_tcp_df$candidate_name == "Russell GORMAN *", "last_name"] <- "GORMAN"
  clean_tcp_df[clean_tcp_df$candidate_name == "Hon John KERIN *", "last_name"] <- "KERIN"
  clean_tcp_df[clean_tcp_df$candidate_name == "Peter Black", "last_name"] <- "BLACK"
  clean_tcp_df[clean_tcp_df$candidate_name == "Geoff Robinson", "last_name"] <- "ROBINSON"
  clean_tcp_df[clean_tcp_df$candidate_name == "Noel HICKS *", "last_name"] <- "HICKS"
  clean_tcp_df[clean_tcp_df$candidate_name == "Alan CADMAN *", "last_name"] <- "CADMAN"
} else if (state == "vic" & year == 1987) {

clean_tcp_df[clean_tcp_df$candidate_name == "ST-APLES *", "last_name"] <- "STAPLES"
} else if (state == "tas" & year == 1987) {

clean_tcp_df[clean_tcp_df$candidate_name == "Christopher MILES *", "last_name"] <- "MILES"
clean_tcp_df[clean_tcp_df$candidate_name == "David Currie", "last_name"] <- "CURRIE"


} else if (state == "nsw" & year == 2001) {


  clean_tcp_df[clean_tcp_df$candidate_name == "Steve Whan", "last_name"] <- "WHAN"
  clean_tcp_df[clean_tcp_df$candidate_name == "Gary NAIRN *", "last_name"] <- "NAIRN"
  clean_tcp_df[clean_tcp_df$candidate_name == "D Williams", "last_name"] <- "WILLIAMS"
}else if (state == "vic" & year == 2001) {

clean_tcp_df[clean_tcp_df$candidate_name == "ST-ONE", "last_name"] <- "STONE"

} else if (state == "sa" & year == 2001) {
  
  clean_tcp_df <- clean_tcp_df %>% 
    filter(division_name != "MAYO")
  
} else if (state == "wa" & year == 2001) {
# new seat## MALLEE is a new division in the text file?
hasluck_df <- data.frame(
  division_name = "HASLUCK",
  candidate_name = c(
    "Ronnie McLean",         
    "Sharryn Jackson",                       
    "Michael Daniels",                         
    "Terry Ryan",                   
    "Luke Edmonds",               
    "Peter Markham",
    "James Hopkinson",
    "Roslyn Hegarty",
    "Bethwyn Chan"),                  
  incumbent = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
  candidate_party = c(NA, "ALP", NA, "CTA", "Grn", "AD", "ON", "NPA", "Lib"),
  vote_count = as.character(c(804, 26891, 520, 1695, 3986, 3455, 4921, 401, 27658)),
  fp_vote_share = c(1.1, 38.2, 0.7, 2.4, 5.7 , 4.9, 7.0, 0.06, 39.3),
  last_name = toupper(c("McLean", 
                        "Jackson", 
                        "Daniels", 
                        "Ryan", 
                        "Edmonds", 
                        "Markham", 
                        "Hopkinson",
                        "Hegarty",
                        "Chan"))
)

# attach to main df
clean_fp_df <- clean_fp_df %>% 
  bind_rows(hasluck_df) %>% 
  arrange(division_name)
}else if (state == "tas" & year == 1990) {

clean_tcp_df[clean_tcp_df$last_name == "S", "last_name"] <- "SMITH"
clean_tcp_df[clean_tcp_df$last_name == "W", "last_name"] <- "SMITH"

}else if (state == "vic" & year == 1990) {
  
  clean_tcp_df[clean_tcp_df$last_name == "ST-APLES", "last_name"] <- "STAPLES"
  

} else if (state == "nsw" & year == 1987) {
  
  clean_tcp_df[clean_tcp_df$last_name == "RUSSELL", "last_name"] <- "GORMAN"
  clean_tcp_df[clean_tcp_df$last_name == "ALAN", "last_name"] <- "CADMAN"
  clean_tcp_df[clean_tcp_df$last_name == "WARREN", "last_name"] <- "MUSGRAVE"
  clean_tcp_df[clean_tcp_df$last_name == "GEOFF", "last_name"] <- "ROBINSON"
  clean_tcp_df[clean_tcp_df$last_name == "NOEL", "last_name"] <- "HICKS"
  clean_tcp_df[clean_tcp_df$last_name == "PETER", "last_name"] <- "BLACK"
  clean_tcp_df[clean_tcp_df$last_name == "DAVID", "last_name"] <- "BROCK"
  clean_tcp_df[clean_tcp_df$last_name == "HON", "last_name"] <- "KERIN"
  
} else if (state == "vic" & year == 1987) {
  
  clean_tcp_df[clean_tcp_df$last_name == "ST-APLES", "last_name"] <- "STAPLES"
} else if (state == "vic" & year == 1984) {
  
  clean_fp_df[clean_fp_df$candidate_name == "Bill de", "last_name"] <- "VINK"
} else if (state == "wa" & year == 1984) {
  
  clean_tcp_df[clean_tcp_df$last_name == "RITA", "last_name"] <- "WATERS"
  clean_tcp_df[clean_tcp_df$last_name == "ALLEN", "last_name"] <- "BLANCHARD"
} else if (state == "nt" & year == 1983) {
  
  clean_tcp_df[clean_tcp_df$last_name == "ROSLYN", "last_name"] <- "KELLY"
  clean_tcp_df[clean_tcp_df$last_name == "JOANNE", "last_name"] <- "HANSEN"
  clean_tcp_df[clean_tcp_df$last_name == "KEVIN", "last_name"] <- "WISE"
  clean_tcp_df[clean_tcp_df$last_name == "ROHAN", "last_name"] <- "GREENLAND"
} else if (state == "tas" & year == 1983) {
  
  clean_tcp_df[clean_tcp_df$last_name == "NEWMAN *", "last_name"] <- "NEWMAN"
  clean_tcp_df[clean_tcp_df$last_name == "GROOM *", "last_name"] <- "GROOM"
} else if (state == "qld" & year == 1983) {
  
  clean_fp_df[clean_fp_df$candidate_name == "Hon Sir James", "last_name"] <- "KILLEN"
} else if (state == "vic" & year == 1983) {

clean_tcp_df[clean_tcp_df$last_name == "ST-APLES", "last_name"] <- "STAPLES"
clean_tcp_df[clean_tcp_df$last_name == "ST-EEDMAN", "last_name"] <- "STEEDMAN"

} else if (state == "nsw" & year == 1983) {
clean_tcp_df[clean_tcp_df$last_name == "NEW", "last_name"] <- "SMITH"

}else if (state == "vic" & year == 1977) {
  clean_tcp_df[clean_tcp_df$last_name == "ST-ALEY", "last_name"] <- "STALEY"

} else if (state == "vic" & year == 1972) {
  clean_tcp_df[clean_tcp_df$last_name == "ST-ALEY", "last_name"] <- "STALEY"
  
} else if (state == "act" & year == 1972) {
  act_df <- data.frame(candidate_name = c(
                         "Keppel Enderby",         
                         "Peter Hughes"
                         ),   
                       other = NA,
                       division_name = "ACT",
                       state = "ACT",
                       year = 1972,
                       last_name = c("ENDERBY", "HUGHES"),
                      tcp_vote_share = as.character(c(52.1, 47.9))
  )
  
  # attach to main df
  clean_tcp_df <- act_df 
} else if (state == "vic" & year == 1966) {
  clean_tcp_df[clean_tcp_df$candidate_name == "ST-REET", "last_name"] <- "STREET"
  clean_tcp_df[clean_tcp_df$candidate_name == "ST-OKES *", "last_name"] <- "STOKES"
} else if (state == "nt" & year == 1963) {
  #nelson ran unopposed in 1963
  nt_df <- data.frame(candidate_name.y = c(
    "John Nelson",         
    NA
  ),   
  other = NA,
  division_name = "NORTHERN TERRITORY",
  state = "NT",
  year = 1963,
  incumbent = 0,
  candidate_party = "ALP",
  vote_count = NA,
  fp_vote_share = NA,
  last_name = c("NELSON", NA),
  tcp_vote_share = as.character(c(100.0, NA))
  )
  
  # attach to main df
  clean_tcp_df <- nt_df 

} else if (state == "vic" & year == 1972) {
  clean_tcp_df[clean_tcp_df$candidate_name == "ST-OKES", "last_name"] <- "STOKES"
  
  
} else if (state == "vic" & year == 1961) {
  clean_tcp_df[clean_tcp_df$candidate_name == "ST-OKES", "last_name"] <- "STOKES"
  
  
}else if (state == "vic" & year == 1958) {
  clean_tcp_df[clean_tcp_df$candidate_name == "ST-OKES", "last_name"] <- "STOKES"
  
  
}else if (state == "vic" & year == 1955) {
  clean_tcp_df[clean_tcp_df$candidate_name == "ST-OKES", "last_name"] <- "STOKES"
  
  
}else if (state == "nt" & year == 1955) {

#nelson ran unopposed in 1963
nt_df <- data.frame(candidate_name.y = c(
  "John Nelson",         
  NA
),   
other = NA,
division_name = "NORTHERN TERRITORY",
state = "NT",
year = 1963,
incumbent = 0,
candidate_party = "ALP",
vote_count = NA,
fp_vote_share = NA,
last_name = c("NELSON", NA),
tcp_vote_share = as.character(c(100.0, NA))
)

# attach to main df
clean_tcp_df <- nt_df 

}

