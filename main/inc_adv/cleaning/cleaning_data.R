rm(list=ls())
library(tidyverse)
library(zoo)

### ------------ANNIE'S TO-DO's --------------- ###
# - add year 2001


#####################################################
####### Cleaning data [input raw data] #########
#####################################################

path <- "~/Dropbox/Thesis/inc_adv/raw_data/federal"
# all years in directory
#list.files(paste0(path, "/lower"))

#----------  a function to import multiple csvs at once ----------# 
readin_files <- function (files, house, year) {
  data <- read_csv(paste0(path, "/", house, "/", year, "/", files), skip = 1)
  return(data)
}

#---------- another one to import and create rename variables ----------# 
readdata_fxn <- function(house, year, data_type) {
  
  # all files in this folder 
  all_year <- list.files(file.path(path, house, year))
  
  # read in all the data
  data <- list()
  data <- sapply(all_year, readin_files, house = house, year = year)
  
  # remove .csv from names
  names(data) <- sapply(1:length(all_year), 
                        function (x) str_split(all_year, pattern = "\\.")[[x]][1])
  
  # clean up a bit
  if (data_type == "party") {
    tpp <- data[[str_which(names(data), pattern = "^tpp_")]]
    out <- tpp %>% 
      # calculating with total votes because we want more precision, decimal places
      mutate(alp_vs = `Australian Labor Party Votes`/ TotalVotes,
             lnp_vs = `Liberal/National Coalition Votes`/TotalVotes,
             alp_margin_t = alp_vs - lnp_vs,
             lnp_margin_t = lnp_vs - alp_vs,
             division = DivisionNm,
             year = year) 
  } else if (data_type == "candidate") {
    tcp <- data[[str_which(names(data), pattern = "^tcp_")]]
    out <- tcp  %>% 
      group_by(DivisionNm) %>% 
      summarize(division_vote_total = sum(TotalVotes)) %>% 
      slice(rep(seq_len(n()), each = 2)) %>% 
      bind_cols(arrange(tcp, DivisionNm)) %>% 
      select(-DivisionNm...1) %>% 
      mutate(tcp_vote_share = TotalVotes / (division_vote_total),
             division = DivisionNm...5,
             year = year) 
  } else if (data_type == "fp") {
    fp <- data[[str_which(names(data), pattern = "^fp_cand")]]
    out <- fp %>% 
      group_by(DivisionNm) %>% 
      summarize(division_vote_total = sum(TotalVotes)) %>% 
      full_join(fp) %>% 
      mutate(fp_vote_share = TotalVotes / (division_vote_total),
             division = DivisionNm,
             year = year) %>% 
      filter(Surname != "Informal")
  }
  
  return(out)
}


#---------- now loop over all the years I have ----------# 
years <- seq(2004, 2019, by = 3)
tpps <- list()
for (i in seq_along(years)){
  tpps[[i]] <- readdata_fxn("lower", years[i], data_type = "party")
}
# rename list of dfs
names(tpps) <- paste0("tpp_federal_", years)

#----------  check that divisions are stable ----------# 
# note that there are ~150 electoral divisions in Australia

# divisions that were eliminated/redistributed or renamed?
#stopifnot(is_empty(setdiff(tpps$tpp_federal_2019$division, tpps$tpsp_federal_2016$division))| 
#          is_empty(setdiff( tpps$tpp_federal_2016$division, tpps$tpp_federal_2019$division)))
# for now, get rid of them...
# (return to this later for better treatment of redistricting)

# create a function that spits out only observations from matching districts
samedist_fxn <- function (year_after, year_before) {
  both <- intersect(year_after$division, year_before$division)
  
  out <- year_after %>% 
    # only divisions common to both years
    filter(division %in% both) %>% 
    bind_rows(filter(year_before, division %in% both))
  
  return(out)
}

# put all the data together
tpp_filtered_data <- samedist_fxn(tpps$tpp_federal_2019, tpps$tpp_federal_2016) %>% 
  bind_rows(samedist_fxn(tpps$tpp_federal_2016, tpps$tpp_federal_2013)) %>% 
  bind_rows(samedist_fxn(tpps$tpp_federal_2013, tpps$tpp_federal_2010))%>% 
  bind_rows(samedist_fxn(tpps$tpp_federal_2010, tpps$tpp_federal_2007))%>% 
  bind_rows(samedist_fxn(tpps$tpp_federal_2007, tpps$tpp_federal_2004)) %>% 
  # remove duplicate years
  distinct()

#----------  create new variables ----------# 

temp_tpp <- tpp_filtered_data %>% 
  # this is variable for win in current year
  mutate(alp_win_t = if_else(alp_vs > 0.5, 1, 0),
         lnp_win_t = if_else(lnp_vs > 0.5, 1, 0))%>% 
  # add incumbency variable
  arrange(division) %>% 
  group_by(division) %>% 
  mutate(
    # variable for win in previous election t-1
    alp_win_t0 = dplyr::lag(alp_win_t, default = NA),
    # variable for win in next year t+1
    alp_win_t1 = dplyr::lead(alp_win_t, default = NA),
    # variable for margin of victory in previous election t-1
    alp_vs_t0 = dplyr::lag(alp_vs, default = NA),
    # variable for margin of victory in next year t+1
    alp_vs_t1 = dplyr::lead(alp_vs, default = NA),
    # variable for incumbent status in current election (means winning last election)
    alp_incumbent = dplyr::lag(alp_win_t, default = NA),
    # do same for liberal-national coalition
    lnp_win_t0 = dplyr::lag(lnp_win_t, default = NA),
    lnp_win_t1 = dplyr::lead(lnp_win_t, default = NA),
    lnp_vs_t0 = dplyr::lag(lnp_vs, default = NA),
    lnp_vs_t1 = dplyr::lead(lnp_vs, default = NA),
    lnp_incumbent = dplyr::lag(lnp_win_t, default = NA)
    )

# I just realized that there's a truncation/censorship problem here?!
# We never account for entry and exit into study...is this a problem for 
# other RDD incumbency studies?
# So, if we have data for years = {2013, 2016, 2019}, we are estimating effect of
# 2013-2016 and 2016-2019, but 2010-2013 and 2019-2022 is truncated?
# If the process that generates the margin of victory in 2013 is not random, 
# is this a concern? 
#

#---------- now, deal with TCP data ----------# 
years <- seq(2004, 2019, by = 3)
tcps <- list()
for (i in seq_along(years)){
  tcps[[i]] <- readdata_fxn("lower", years[i], data_type = "candidate")
}
# rename list of dfs
names(tcps) <- paste0("tcp_federal_", years)


# put all years together 
tcp_filtered_data <- samedist_fxn(tcps$tcp_federal_2019, tcps$tcp_federal_2016) %>% 
  bind_rows(samedist_fxn(tcps$tcp_federal_2016, tcps$tcp_federal_2013)) %>% 
  bind_rows(samedist_fxn(tcps$tcp_federal_2013, tcps$tcp_federal_2010))%>% 
  bind_rows(samedist_fxn(tcps$tcp_federal_2010, tcps$tcp_federal_2007))%>% 
  bind_rows(samedist_fxn(tcps$tcp_federal_2007, tcps$tcp_federal_2004)) %>% 
  # remove duplicate years
  distinct()

#----------  create new variables ----------# 
temp_tcp <- tcp_filtered_data %>% 
  group_by(division, year) %>% 
  mutate(candidate_margin_t = tcp_vote_share - lag(tcp_vote_share, default = first(tcp_vote_share)),
         candidate_margin_t = if_else(candidate_margin_t==0, lead(candidate_margin_t)*(-1), candidate_margin_t)) %>% 
  arrange(division, desc(year)) %>% 
  mutate(div_year_id = 1:n()) %>%
  group_by(division) %>%
  mutate(surname_t0 = ifelse(div_year_id == 1,
                         ifelse(str_detect(Surname, lead(Surname, n = 2L)) | str_detect(Surname, lead(Surname, n = 3L)), 1, 0),
                         ifelse(str_detect(Surname, lead(Surname, n = 1L)) | str_detect(Surname, lead(Surname, n = 2L)), 1, 0)),
         surname_t1 = ifelse(div_year_id == 1,
                             ifelse(str_detect(Surname, lag(Surname, n = 1L)) | str_detect(Surname, lag(Surname, n = 2L)), 1, 0),
                             ifelse(str_detect(Surname, lag(Surname, n = 2L)) | str_detect(Surname, lag(Surname, n = 3L)), 1, 0))) %>%
  mutate(tcp_t0 = ifelse(surname_t0 == 1, 
                         ifelse(div_year_id == 1, ifelse(str_detect(Surname, lead(Surname, n = 3L)), 
                                                         lead(tcp_vote_share, n = 3L),
                                                         lead(tcp_vote_share, n = 2L)), 
                                ifelse(div_year_id == 2, ifelse(str_detect(Surname, lead(Surname, n = 2L)), 
                                                                lead(tcp_vote_share, n = 2L), 
                                                                lead(tcp_vote_share, n = 1L)), 
                                                                NA)),
                         NA),
         tcp_t1 = ifelse(surname_t1 == 1, 
                         ifelse(div_year_id == 1, ifelse(str_detect(Surname, lag(Surname, n = 2L)), 
                                                         lag(tcp_vote_share, n = 2L),
                                                         lag(tcp_vote_share, n = 1L)), 
                                ifelse(div_year_id == 2, ifelse(str_detect(Surname, lead(Surname, n = 3L)), 
                                                                lag(tcp_vote_share, n = 3L), 
                                                                lag(tcp_vote_share, n = 2L)), 
                                       NA)),
                         NA)) %>% 
  select(-Surname, everything()) 

# check if candidate ran in election at t-1 -- need first preference data for this

#---------- now, deal with FP data ----------# 
years <- seq(2004, 2019, by = 3)
fps <- list()
for (i in seq_along(years)){
  fps[[i]] <- readdata_fxn("lower", years[i], data_type = "fp")
}
# rename list of dfs
names(fps) <- paste0("fp_federal_", years)

# put all years together 
fp_filtered_data <- samedist_fxn(fps$fp_federal_2019, fps$fp_federal_2016) %>% 
  bind_rows(samedist_fxn(fps$fp_federal_2016, fps$fp_federal_2013)) %>% 
  bind_rows(samedist_fxn(fps$fp_federal_2013, fps$fp_federal_2010))%>% 
  bind_rows(samedist_fxn(fps$fp_federal_2010, fps$fp_federal_2007))%>% 
  bind_rows(samedist_fxn(fps$fp_federal_2007, fps$fp_federal_2004)) %>% 
  # remove duplicate years
  distinct() %>% 
  group_by(division, year) %>% 
  arrange(division, desc(year)) 


#------------ MERGE TCP WITH FP DATA  ----------# 

tcp_new <- fp_filtered_data %>% 
  select(division, year, fp_vote_share, Surname) %>% 
  right_join(temp_tcp, by = c("division", "year", "Surname")) %>% 
  #only variables we want, maybe can do something with these variables later...
  select(-c(DivisionNm, DivisionID, CandidateID, BallotPosition,
            HistoricElected, Elected, PartyNm, OrdinaryVotes, 
            AbsentVotes, ProvisionalVotes, PrePollVotes, 
            PostalVotes, SittingMemberFl, Swing)) %>%
  mutate(PartyAb = case_when(PartyAb == "ALP" ~ "ALP",
                             PartyAb %in% list("NP", "LP", "LNP", "LNQ", "CLP") ~ "LNP",
                             PartyAb == "GRN" ~ "GRN",
                             PartyAb == "ON" ~ "ON",
                             PartyAb == "XEN" ~ "XEN",
                             PartyAb == "IND" ~ "IND",
                             PartyAb == "PUP" ~ "PUP",
                             PartyAb == "KAP" ~ "KAP"),
         incumbent = ifelse(surname_t0 == 1 & tcp_t0 > 0.50, 1, 0),
         alp_incumbent = ifelse(incumbent == 1 & PartyAb == "ALP", 1, 0),
         lnp_incumbent = ifelse(incumbent == 1 & PartyAb == "LNP", 1, 0)) %>% 
  group_by(division, year) %>% 
  mutate(open_seat = ifelse(incumbent == 0 & (lag(incumbent == 0) | lead(incumbent == 0)), 1, 0),
         open_seat = ifelse(is.na(open_seat) & !is.na(incumbent), 0, open_seat))

View(tcp_new)

#------------ MERGE TPP WITH FP DATA  ----------# 

tpp_new <- tcp_new %>% 
  select(division, year, fp_vote_share, tcp_vote_share, Surname, PartyAb, open_seat) %>% 
  inner_join(temp_tpp, by = c("division", "year")) %>% 
  mutate(alp_fp = ifelse(PartyAb.x == "ALP", fp_vote_share, NA), 
         lnp_fp = ifelse(PartyAb.x == "LNP", fp_vote_share, NA)) %>% 
  group_by(division, year) %>% 
  fill(alp_fp, .direction = "downup") %>% 
  fill(lnp_fp, .direction = "downup") %>% 
  filter(PartyAb.x == "ALP") %>% 
  group_by(division) %>% 
  # add fp lagged variables
  mutate(alp_fp_t0 = dplyr::lag(alp_fp, default = NA),
         alp_fp_t1 = dplyr::lead(alp_fp, default = NA),
         lnp_fp_t0 = dplyr::lag(lnp_fp, default = NA),
         lnp_fp_t1 = dplyr::lead(lnp_fp, default = NA)) %>% 
  ungroup() %>% 
  rename(PartyAb = PartyAb.x) %>% 
  mutate(DivisionNm = toupper(DivisionNm)) %>% 
  select(-c(division, PartyAb.y)) 

View(tpp_new)

#------------ SAVE CLEAN DATA  ----------# 

write.csv(tpp_new, "~/Dropbox/Thesis/inc_adv/clean_data/tpp_data.csv")
write.csv(tcp_new, "~/Dropbox/Thesis/inc_adv/clean_data/tcp_data.csv")
