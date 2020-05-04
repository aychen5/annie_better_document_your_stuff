library(tidyverse)

#####################################################
####### Cleaning data [input raw data] #########
#####################################################

path <- "~/Dropbox/Thesis/inc_adv/data/federal"
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
      # this only gives me two decimal places
      mutate(alp_margin_t = `Australian Labor Party Percentage` - `Liberal/National Coalition Percentage`,
             alp_vs = `Australian Labor Party Percentage`,
             division = DivisionNm,
             year = year) 
  } else if (data_type == "candidate") {
    tcp <- data[[str_which(names(data), pattern = "^tcp_")]]
    out <- tcp %>% 
      # this only gives me two decimal places
      mutate(alp_margin_t = `Australian Labor Party Percentage` - `Liberal/National Coalition Percentage`,
             alp_vs = `Australian Labor Party Percentage`,
             division = DivisionNm,
             year = year) 
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
stopifnot(is_empty(setdiff(tpps$tpp_federal_2019$division, tpps$tpp_federal_2016$division))| 
          is_empty(setdiff( tpps$tpp_federal_2016$division, tpps$tpp_federal_2019$division)))

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
filtered_data <- samedist_fxn(tpps$tpp_federal_2019, tpps$tpp_federal_2016) %>% 
  bind_rows(samedist_fxn(tpps$tpp_federal_2016, tpps$tpp_federal_2013)) %>% 
  bind_rows(samedist_fxn(tpps$tpp_federal_2013, tpps$tpp_federal_2010))%>% 
  bind_rows(samedist_fxn(tpps$tpp_federal_2010, tpps$tpp_federal_2007))%>% 
  bind_rows(samedist_fxn(tpps$tpp_federal_2007, tpps$tpp_federal_2004)) %>% 
  # remove duplicate years
  distinct()

#----------  create new variables ----------# 

tpp_data <- filtered_data %>% 
  # this is variable for win in current year
  mutate(alp_win_t = if_else(alp_margin_t > 0, 1, 0))%>% 
  # add incumbency variable
  arrange(division) %>% 
  group_by(division) %>% 
  mutate(
    # variable for win in previous election t-1
    alp_win_t0 = dplyr::lag(alp_win_t, default = 0),
    # variable for win in next year t+1
    alp_win_t1 = dplyr::lead(alp_win_t, default = 0),
    # variable for margin of victory in previous election t-1
    alp_margin_t0 = dplyr::lag(alp_margin_t, default = NA),
    # variable for margin of victory in next year t+1
    alp_margin_t1 = dplyr::lead(alp_margin_t, default = NA),
    # variable for incumbent status in current election
    incumbent = dplyr::lead(alp_win_t, default = 0)
    )

# save clean TPP data

write.csv(tpp_data, "~/Dropbox/Thesis/inc_adv/clean_data/tpp_data.csv")

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













