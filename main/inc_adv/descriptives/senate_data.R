library(readr)
library(tidyverse)
library("CGPfunctions")

path <- "~/Dropbox/Thesis/inc_adv/raw_data/federal/upper"
readin_files <- function (year) {
  data <- read_csv(paste0(path, "/fp_senate_", year, ".csv"),
                   skip = 1)
  return(data)
}

years <-  seq(from = 2004, to = 2019, by = 3)
data <- map(years, readin_files)
names(data) <- paste0("fp_senate_", years)


# compute division-level totals and % for each party
clean_senate <- function(data) {
  out <- data %>%
    dplyr::rename(division=DivisionNm) %>%
    group_by(division,PartyAb) %>%
    summarise(total=sum(TotalVotes),
              State=StateAb[1]) %>%
    group_by(division) %>%
    mutate(p = total/sum(total)*100)
  return(out)
}
senate <- map(data, clean_senate)


# map senate and house party names and make them consistent across division
labs <- list()
labs[["NSW"]] <- data.frame(LP="LNP",
                            NP="LNP",
                            LPNP="LNP",
                            ALP="ALP",
                            GRN="GRN")
labs[["VIC"]] <- labs[["NSW"]]
labs[["QLD"]] <- data.frame(LNP="LNP",
                            ALP="ALP",
                            GRN="GRN")
labs[["TAS"]] <- data.frame(LP="LNP",ALP="ALP",GRN="GRN")
labs[["SA"]] <- labs[["TAS"]]
labs[["WA"]] <- data.frame(LP="LNP",NP="LNP",ALP="ALP",GRN="GRN")
labs[["NT"]] <- data.frame(CLP="LNP",
                           ALP="ALP",
                           GRN="GRN")
labs[["ACT"]] <- labs[["TAS"]]

labs <- bind_rows(labs,.id="State") %>%
  group_by(State) %>%
  gather(PartyAb,Party,-State) %>%
  ungroup() %>%
  filter(!is.na(Party))

# merge with original senate data
merge_senate <- function (data) {
  data <- left_join(data, labs,by=c("State","PartyAb")) %>%
    group_by(State,division,Party) %>%
    summarise(p = sum(p)) %>%
    filter(Party %in% c("LNP","LP","NP","ALP","GRN","CLP"))
  return(data)
}

all_senate <- map(senate, merge_senate)

# now, reformat the house results a little
# to get fp data by division by vote type,
source("./cleaning/cleaning_data.R")

house_fxn <- function (data, senate_df) {
  out <- data %>%
    rename(State=StateAb) %>%
    filter(Surname!="Informal") %>%
    group_by(division) %>%
    mutate(Party=PartyAb,
           per=TotalVotes/sum(TotalVotes)*100) %>%
    select(State,division,Party,
           Surname,GivenNm, 
           Incumbent=HistoricElected, per) %>%
    mutate(Party = if_else(State!="WA",
                         dplyr::recode(Party,
                                `LP`="LNP",
                                `NP`="LNP",
                                `LNP`="LNP",
                                `CLP`="LNP"),
                         Party)) %>%
    left_join(senate_df, by=c("State","division","Party")) %>% 
    filter(Party %in% c("ALP","LP","NP","LNP","GRN")) %>%
    mutate(pvote = per - p)
  return(out)
}
 
snt_hse <- house_fxn(data = fps$fp_federal_2019,
           senate_df = all_senate$fp_senate_2019)

# drop Coalition House candidates in three-cornered contests 
# but where the Coalition ran a joint Senate ticket
tcc <- snt_hse %>% 
  group_by(division,Party) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(Party=="LNP",n>1)

snt_hse <- anti_join(snt_hse,
                  tcc,
                  by=c("division","Party"))


glimpse(snt_hse)

### plot senate vs house: data points on the left show Senate votes for the
#ALP in House seats with ALP incumbents; on the right hand side of
#the graph are the corresponding share of 1st preferences for Coalition
#incumbents. The lines connecting each pair of data points tends to slope up,
#indicating that on average, Coalition House candidates outperformed the Senate
#ticket in their respective seats.

slopegraph_df <- 
  snt_hse %>% 
  ungroup() %>%
  filter(Party=="LNP" & 
           Incumbent=="Y") %>%
  dplyr::rename(Senate=p ,House=per) %>%
  #group_by(Senate, House) %>% 
  pivot_longer(-c(division, State, Party, Surname, GivenNm, Incumbent,
                  pvote), 
               names_to = "type", values_to = "fp") %>% 
  dplyr::mutate(name=stringr::str_to_title(Surname),
                indx=1:n(),
                fp = round(fp, digits = 2)) %>%
  arrange(division)




# create a slope graph
plot_fxn <- function (year, data) {
  out <- newggslopegraph(dataframe = slopegraph_df, 
                         Times = type, 
                         Measurement = fp, 
                         Grouping = division, 
                         Title = "Personal Vote for Coalition Incumbents", 
                         SubTitle = paste0(year, " Australian Federal Election"), 
                         Caption = NULL,
                         DataTextColor = "#337AB7",
                         DataLabelPadding = 0.05,
                         DataTextSize = 3,
                         LineThickness = .35,
                         YTextSize = 2,
                         LineColor = "salmon",
                         ThemeChoice = "gdocs"
  )
  return(out)
}

years
slopegrph_2019 <- plot_fxn(data = slopegraph_df, year = 2019)

ggsave("slopegrph_2019.png")
