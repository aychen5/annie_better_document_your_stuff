pacman::p_load(tidyverse, readxl)


read_state_fxn <- function (state, year, type = c("2CP", "FP", "TPP")) {
  excel_type <- ifelse(state %in% c("WA", "SA") & year %in% c(2017, 2014), ".csv", ".xlsx") 
  
  file_name <- paste0(paste0(c(state, year, type, "Electorate"), collapse = "-"), excel_type)
  
  if(excel_type == ".xlsx") {
    df <- read_xlsx(paste0("~/Dropbox/Thesis/inc_adv/raw_data/state/", tolower(state), "/", file_name))
  } else {
    df <- read_csv(paste0("~/Dropbox/Thesis/inc_adv/raw_data/state/", tolower(state), "/", file_name))
  }
  
  df <- df %>% 
    mutate(year = year,
           state = state) %>% 
    filter(!candidate_name %in% c("Informal", "Exhausted"))
  
  return(df)
}

### ------- all tcp data --------- ###
states_tcp_fxn <- function(year_vector, state) {
  df <- map(year_vector, 
      function (x) read_state_fxn(state = state, year = x, type = "2CP")) 
  if (state == "VIC") {
    df[[1]] <- df[[1]] %>% select(-c(region_name, region_id, percent))
  } else if (state == "WA") {
    df[[1]] <- df[[1]] %>% select(-c(formal_votes))
  } else {df <- df}
  
  out <- df %>% 
    do.call(what = rbind) %>% 
    arrange(district_name, desc(year)) 
  return(out)
}

nsw_tcp <- states_tcp_fxn( year_vector = c(2019, 2015, 2011), state = "NSW")
qld_tcp <- states_tcp_fxn( year_vector = c(2017, 2015, 2012, 2009, 2004), state = "QLD")  
sa_tcp <- states_tcp_fxn( year_vector = c(2018, 2014), state = "SA")  
nt_tcp <- states_tcp_fxn( year_vector = c(2016, 2012), state = "NT")  
wa_tcp <- states_tcp_fxn( year_vector = c(2017, 2013), state = "WA")  
vic_tcp <- states_tcp_fxn( year_vector = c(2018, 2014), state = "VIC") 

# put all states together and clean up a bit
temp_tcp_states <- bind_rows(nsw_tcp, qld_tcp, sa_tcp, nt_tcp, wa_tcp, vic_tcp) %>% 
  mutate(surname = toupper(surname))
tcp_states <- temp_tcp_states %>% 
  group_by(district_name, year) %>% 
  summarize(district_vote_total = sum(votes)) %>% 
  left_join(temp_tcp_states, by = c("district_name", "year")) %>% 
  mutate(state_tcp_vote = votes/district_vote_total) %>% 
  arrange(district_name, desc(year))



### ------- all fp data --------- ###

states_fp_fxn <- function(year_vector, state) {
  df <- map(year_vector, 
            function (x) read_state_fxn(state = state, year = x, type = "FP")) 
  if (state == "VIC") {
    df[[1]] <- df[[1]] %>% select(-c(region_name, region_id, percent))
    df[[2]] <- df[[2]] %>% select(-c(region_name, region_id))
  } else {df <- df}
  
  out <- df %>% 
    do.call(what = rbind) %>% 
    arrange(district_name, desc(year)) 
  return(out)
}

nsw_fp <- states_fp_fxn( year_vector = c(2019, 2015, 2011), state = "NSW")
qld_fp <- states_fp_fxn( year_vector = c(2017, 2015, 2012, 2009, 2004), state = "QLD")  
sa_fp <- states_fp_fxn( year_vector = c(2018, 2014), state = "SA")  
nt_fp <- states_fp_fxn( year_vector = c(2016, 2012), state = "NT")  
wa_fp <- states_fp_fxn( year_vector = c(2017, 2013), state = "WA")  
vic_fp <- states_fp_fxn( year_vector = c(2018, 2014), state = "VIC") 

# put all states together
temp_fp_states <- bind_rows(nsw_fp, qld_fp, sa_fp, nt_fp, wa_fp, vic_fp) %>% 
  mutate(surname = toupper(surname)) 
fp_states <- temp_fp_states %>% 
  group_by(district_name, year) %>% 
  summarize(district_vote_total = sum(votes)) %>% 
  left_join(temp_fp_states, by = c("district_name", "year")) %>% 
  mutate(state_fp_vote = votes/district_vote_total) %>% 
  arrange(district_name, desc(year))


### ------------- merge fp data to tcp data and add rdd variables ----------- ###
tcp_states_data <- fp_states %>% 
  select(district_name, year, state_fp_vote, surname) %>% 
  right_join(tcp_states, by = c("district_name", "year", "surname")) %>%  
  group_by(district_name, year) %>% 
  mutate(state_candidate_margin_t = state_tcp_vote - lag(state_tcp_vote, default = first(state_tcp_vote)),
         state_candidate_margin_t = if_else(state_candidate_margin_t==0, 
                                            lead(state_candidate_margin_t)*(-1), 
                                            state_candidate_margin_t)) %>% 
  arrange(district_name, desc(year)) %>% 
  mutate(div_year_id = 1:n()) %>%
  group_by(district_name) %>%
  mutate(state_surname_t0 = ifelse(div_year_id == 1,
                             ifelse(str_detect(surname, lead(surname, n = 2L)) | str_detect(surname, lead(surname, n = 3L)), 1, 0),
                             ifelse(str_detect(surname, lead(surname, n = 1L)) | str_detect(surname, lead(surname, n = 2L)), 1, 0)),
         state_surname_t1 = ifelse(div_year_id == 1,
                             ifelse(str_detect(surname, lag(surname, n = 1L)) | str_detect(surname, lag(surname, n = 2L)), 1, 0),
                             ifelse(str_detect(surname, lag(surname, n = 2L)) | str_detect(surname, lag(surname, n = 3L)), 1, 0))) %>%
  mutate(state_tcp_t0 = ifelse(state_surname_t0 == 1, 
                         ifelse(div_year_id == 1, ifelse(str_detect(surname, lead(surname, n = 3L)), 
                                                         lead(state_tcp_vote, n = 3L),
                                                         lead(state_tcp_vote, n = 2L)), 
                                ifelse(div_year_id == 2, ifelse(str_detect(surname, lead(surname, n = 2L)), 
                                                                lead(state_tcp_vote, n = 2L), 
                                                                lead(state_tcp_vote, n = 1L)), 
                                       NA)),
                         NA),
         state_tcp_t1 = ifelse(state_surname_t1 == 1, 
                         ifelse(div_year_id == 1, ifelse(str_detect(surname, lag(surname, n = 2L)), 
                                                         lag(state_tcp_vote, n = 2L),
                                                         lag(state_tcp_vote, n = 1L)), 
                                ifelse(div_year_id == 2, ifelse(str_detect(surname, lead(surname, n = 3L)), 
                                                                lag(state_tcp_vote, n = 3L), 
                                                                lag(state_tcp_vote, n = 2L)), 
                                       NA)),
                         NA)) %>% 
  mutate(incumbent = ifelse(state_surname_t0 == 1 & state_tcp_t0 > 0.50, 1, 0)) %>% 
  group_by(district_name, year) %>% 
  mutate(open_seat = ifelse(incumbent == 0 & (lag(incumbent == 0) | lead(incumbent == 0)), 1, 0),
         open_seat = ifelse(is.na(open_seat) & !is.na(incumbent), 0, open_seat)) %>% 
  select(-surname, everything()) 

#View(tcp_states_data)

### ------------- convert tcp to tpp data ----------- ###

tpp_states_data <- tcp_states_data %>% 
  mutate(partyab = case_when(party_code == "ALP" ~ "ALP",
                             party_code %in% list("LIB", "NAT", "CLP", "LNP", "NPA") ~ "LNP",
                             TRUE ~ "OTH")) %>% 
  mutate(state_alp_margin_t = ifelse(partyab == "ALP", state_candidate_margin_t, NA),
         state_lnp_margin_t = ifelse(partyab == "LNP", state_candidate_margin_t, NA),
         state_alp_vs = ifelse(partyab == "ALP", state_tcp_vote, NA),
         state_lnp_vs = ifelse(partyab == "LNP", state_tcp_vote, NA),
         state_alp_fp = ifelse(partyab == "ALP", state_fp_vote, NA),
         state_lnp_fp = ifelse(partyab == "LNP", state_fp_vote, NA)) %>% 
  group_by(district_name, year) %>%  
  fill(state_alp_vs, .direction = "downup") %>% 
  fill(state_lnp_vs, .direction = "downup") %>% 
  fill(state_alp_margin_t, .direction = "downup") %>% 
  fill(state_lnp_margin_t, .direction = "downup") %>% 
  fill(state_alp_fp, .direction = "downup") %>% 
  fill(state_lnp_fp, .direction = "downup") %>% 
  mutate(state_alp_win_t = ifelse(state_alp_vs > 0.5, 1, 0),
         state_lnp_win_t = ifelse(state_lnp_vs > 0.5, 1, 0)) %>% 
  filter(partyab == "ALP") %>% 
  arrange(district_name, desc(year)) %>% 
  group_by(district_name) %>% 
  mutate( state_alp_win_t0 = dplyr::lag(state_alp_win_t, default = NA),
          state_alp_win_t1 = dplyr::lead(state_alp_win_t, default = NA),
          state_alp_vs_t0 = dplyr::lag(state_alp_vs, default = NA),
          state_alp_vs_t1 = dplyr::lead(state_alp_vs, default = NA),
          state_alp_fp_t1 = dplyr::lead(state_alp_fp, default = NA),
          state_alp_incumbent = dplyr::lag(state_alp_win_t, default = NA),
          # do same for liberal-national coalition
          state_lnp_win_t0 = dplyr::lag(state_lnp_win_t, default = NA),
          state_lnp_win_t1 = dplyr::lead(state_lnp_win_t, default = NA),
          state_lnp_vs_t0 = dplyr::lag(state_lnp_vs, default = NA),
          state_lnp_vs_t1 = dplyr::lead(state_lnp_vs, default = NA),
          state_lnp_fp_t1 = dplyr::lead(state_lnp_fp, default = NA),
          state_lnp_incumbent = dplyr::lag(state_lnp_win_t, default = NA)
  )

### ------- save data --------- ###
write.csv(tpp_states_data, "~/Dropbox/Thesis/inc_adv/clean_data/tpp_state_data.csv")
write.csv(tcp_states_data, "~/Dropbox/Thesis/inc_adv/clean_data/tcp_state_data.csv")
write.csv(fp_states, "~/Dropbox/Thesis/inc_adv/clean_data/fp_state_data.csv")



