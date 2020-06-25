library(tidyverse)
library(zoo)

path <- "~/Dropbox/Thesis/inc_adv/clean_data"

read_old_fxn <- function (year) {
  
  #type <- ifelse(data_type == "fp", "fp-", "tcp-")
                 
  all_states <- list.files(file.path(path, year), pattern = "fp-")
  
  # read in all the data
  readin_files <- function (file, year) {
    data <- read_csv(paste0(path, "/", year, "/", file))
    return(data)
  }
  data <- list()
  data <- lapply(all_states, readin_files, year = year)
  
  # remove .csv from names
  # names(all_data) <- sapply(1:length(all_states), 
  #                           function (x) str_split(all_states, pattern = "\\.")[[x]][1])
  all_data <- do.call(rbind, data)
  
  return(all_data)
}

#1951, 1955, 1958, 961, 1963, 1966, 1972, 1975, 1977, 1980, 1983, 1984, 1987,
#1990 1993 1996 1998 2001
years <- c(1980, 1983, seq(from = 1984, to = 1998, by = 3), 1998, 2001)

all_data <- map(years, read_old_fxn) %>% 
  do.call(what = rbind) %>% 
  arrange(division_name, desc(year)) %>% 
  rename(StateAb = state, 
         division = division_name, 
         Surname = last_name,
         GivenNm = candidate_name.y) %>% 
  mutate(PartyAb = case_when(candidate_party == "ALP" ~ "ALP",
                      candidate_party %in% list("NPA", "Lib", "CLP") ~ "LNP",
                      candidate_party == "Grn" ~ "GRN",
                      candidate_party == "ON" ~ "ON"),
         tcp_vote_share = as.numeric(tcp_vote_share))  

# add rdd variables
tcp_old <- all_data %>% 
  group_by(division, year) %>% 
  summarize(division_vote_total = sum(vote_count)) %>% 
  left_join(all_data, by = c("division", "year")) %>% 
  mutate(precise_tcp_vote = vote_count/division_vote_total,
         div_year_id = NA,
         tcp_vote_share = tcp_vote_share/100,
         fp_vote_share = fp_vote_share/100) %>%  
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
  rename(TotalVotes = vote_count) %>% 
  mutate(incumbent = ifelse(surname_t0 == 1 & tcp_t0 > 0.50, 1, 0)) %>% 
  group_by(division, year) %>% 
  mutate(open_seat = ifelse(incumbent == 0 & (lag(incumbent == 0) | lead(incumbent == 0)), 1, 0),
         open_seat = ifelse(is.na(open_seat) & !is.na(incumbent), 0, open_seat)) %>% 
  select(-Surname, everything()) 

### merge with new tcp data
tcp_data <- tcp_data %>% 
  bind_rows(tcp_old) %>% 
  mutate(division = toupper(division)) %>% 
  arrange(division, desc(year)) %>% 
  unite("division-year", division, year, sep = "-",  remove = FALSE)

#------------ GET FP DATA ----------# 

years <- c(1980, 1983, seq(from = 1984, to = 1998, by = 3), 1998, 2001)
fp_old <- map(years, function(year) {read_old_fxn(year)}) %>% 
  do.call(what = rbind) %>% 
  arrange(division_name, desc(year)) %>% 
  rename(division = division_name,
         Surname = last_name,
         DivisionID = division_id,
         Swing = swing,
         PartyAb = candidate_party,
         TotalVotes = vote_count,
         StateAb = state,
         GivenNm = candidate_name) %>% 
  mutate(Swing = as.numeric(str_extract(Swing, pattern = "(\\-)?[0-9]+\\.[0-9]+")),
         fp_vote_share = fp_vote_share/100)

fp_new <- fp_filtered_data %>% 
  mutate(incumbent = ifelse(HistoricElected == "Y", 1, 0)) %>% 
  select(-c(BallotPosition, CandidateID, Elected, 
            PostalVotes, ProvisionalVotes, AbsentVotes,
            OrdinaryVotes, PartyNm, PrePollVotes,HistoricElected,
            SittingMemberFl, DivisionNm, division_vote_total))

# merge with fp new
fp_data <- bind_rows(fp_new, fp_old) %>% 
  arrange(division, desc(year))  %>% 
  mutate(division = toupper(division)) %>% 
  unite("division-year", division, year, sep = "-",  remove = FALSE)


#------------ GET TPP DATA FROM TCP  ----------# 

tpp_old <- tcp_old %>% 
  rename(DivisionNm = division) %>% 
  mutate(alp_margin_t = ifelse(PartyAb == "ALP", candidate_margin_t, NA),
         lnp_margin_t = ifelse(PartyAb == "LNP", candidate_margin_t, NA),
         alp_vs = ifelse(PartyAb == "ALP", tcp_vote_share, NA),
         lnp_vs = ifelse(PartyAb == "LNP", tcp_vote_share, NA),
         alp_fp = ifelse(PartyAb == "ALP", fp_vote_share, NA),
         lnp_fp = ifelse(PartyAb == "LNP", fp_vote_share, NA),
         DivisionID = NA, 
         Swing = NA) %>% 
  group_by(DivisionNm, year) %>%  
  fill(alp_vs, .direction = "downup") %>% 
  fill(lnp_vs, .direction = "downup") %>% 
  fill(alp_margin_t, .direction = "downup") %>% 
  fill(lnp_margin_t, .direction = "downup") %>% 
  fill(alp_fp, .direction = "downup") %>% 
  fill(lnp_fp, .direction = "downup") %>% 
  mutate(alp_win_t = ifelse(alp_margin_t > 0, 1, 0),
         lnp_win_t = ifelse(lnp_margin_t > 0, 1, 0)) %>% 
  filter(PartyAb == "ALP") %>% 
  group_by(DivisionNm) %>% 
  mutate( alp_win_t0 = dplyr::lag(alp_win_t, default = 0),
          alp_win_t1 = dplyr::lead(alp_win_t, default = 0),
          alp_vs_t0 = dplyr::lag(alp_vs, default = NA),
          alp_vs_t1 = dplyr::lead(alp_vs, default = NA),
          alp_fp_t1 = dplyr::lead(alp_fp, default = NA),
          alp_incumbent = dplyr::lead(alp_win_t, default = 0),
          # do same for liberal-national coalition
          lnp_win_t0 = dplyr::lag(lnp_win_t, default = 0),
          lnp_win_t1 = dplyr::lead(lnp_win_t, default = 0),
          lnp_vs_t0 = dplyr::lag(lnp_vs, default = NA),
          lnp_vs_t1 = dplyr::lead(lnp_vs, default = NA),
          lnp_fp_t1 = dplyr::lead(lnp_fp, default = NA),
          lnp_incumbent = dplyr::lead(lnp_win_t, default = 0)
          )

### merge to new tpp data (after 2001)
tpp_data <- tpp_data %>% 
  select(-c(setdiff(colnames(tpp_data), colnames(tpp_old)))) %>% 
  bind_rows(tpp_old) %>% 
  arrange(DivisionNm)

View(tpp_old)
View(tpp_data)
# ---------- add ideology data ------------------- #

mrp_data <- read_csv("~/Dropbox/Thesis/inc_adv/clean_data/mrp.csv") %>% 
  mutate(DivisionNm = toupper(division),
         division = toupper(division)) %>% 
  select(c(model_div_pref, DivisionNm, division))

all_tpp <- merge(tpp_data, mrp_data %>% select(c(model_div_pref, DivisionNm)), by = "DivisionNm")
all_tcp <- merge(tcp_data, mrp_data %>% select(c(model_div_pref, division)), by = "division")
all_fp <- merge(fp_data, mrp_data %>% select(c(model_div_pref, division)), by = "division")




#------------ SAVE CLEAN DATA  ----------# 

write.csv(all_tpp, "~/Dropbox/Thesis/inc_adv/clean_data/tpp_data.csv")
write.csv(all_tcp, "~/Dropbox/Thesis/inc_adv/clean_data/tcp_data.csv")
write.csv(all_fp, "~/Dropbox/Thesis/inc_adv/clean_data/fp_data.csv")

