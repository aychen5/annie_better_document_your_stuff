library(tidyverse)

#####################################################
####### Cleaning data [input raw data] #########
#####################################################

path <- "~/Dropbox/Thesis/inc_adv/data/federal"

#----------  a function to import multiple csvs at once ----------# 
readin_files <- function (files, house, year) {
  data <- read_csv(paste0(path, "/", house, "/", year, "/", files), skip = 1)
  return(data)
}

#---------- another one to import and clean up ----------# 
tpp_fxn <- function(house, year) {
  
  # all files in this folder 
  all_year <- list.files(file.path(path, house, year))
  
  # read in all the data
  data <- list()
  data <- sapply(all_year, readin_files, house = house, year = year)
  
  # remove .csv from names
  names(data) <- sapply(1:length(all_year), 
                             function (x) str_split(all_year, pattern = "\\.")[[x]][1])
  
  # begin with only tpp data
  tpp <- data[[str_which(names(data), pattern = "^tpp_")]]
  
  # clean up a bit
  tpp_federal_year <- tpp %>% 
    # this only gives me two decimal places
    mutate(alp_margin = `Australian Labor Party Percentage` - `Liberal/National Coalition Percentage`,
           alp_vs = `Australian Labor Party Percentage`,
           division = DivisionNm,
           year = year)
  
  return(tpp_federal_year)
}

#---------- now loop over all the years I have ----------# 
years <- seq(2001, 2019, by = 3)
tpps <- list()
for (i in seq_along(years)){
  tpps[[i]] <- tpp_fxn("lower", years[i])
}
#tpp_federal_2019 <- tpp_fxn("lower", 2019)
#tpp_federal_2016 <- tpp_fxn("lower", 2016)

#----------  check that divisions are stable ----------# 
# divisions that were eliminated/redistributed or renamed?
stopifnot(is_empty(setdiff(tpp_federal_2019$division, tpp_federal_2016$division))| 
          is_empty(setdiff(tpp_federal_2016$division, tpp_federal_2019$division)))

# for now, get rid of them...(return to this later)
both <- intersect(tpp_federal_2019$division, tpp_federal_2016$division)

#----------  create new variables ----------# 
all_data <- tpp_federal_2019 %>% 
  # only divisions common to both years
  filter(division %in% both) %>% 
  bind_rows(filter(tpp_federal_2016, division %in% both)) %>% 
  # create win variable
  mutate(alp_win_t = case_when(alp_margin > 0 & year == 2016 ~ 1))%>% 
  # add incumbency variable
  arrange(division) %>% 
  group_by(division) %>% 
  mutate(incumbent = na_if(dplyr::lead(alp_win_t, default = 0), 0),
         alp_win_t1 = case_when(alp_margin > 0 & year == 2019 ~ 1))


#glimpse(all_data)


### begin to check imbalance

#66
table(all_data$incumbent)
#61
all_data %>% 
  filter(incumbent == 1 & alp_win_t1 == 1) %>% 
  nrow()

ggplot(all_data[all_data$incumbent == 1,]) +
  geom_density(aes(alp_vs)) +
  geom_vline(xintercept = mean(all_data[all_data$incumbent == 1,]$alp_vs, na.rm = T), 
             lty = 2, col = "red", lwd = 1.5) + 
  theme_minimal()

#mean
57.98788/100
#sd
6.695208/100

