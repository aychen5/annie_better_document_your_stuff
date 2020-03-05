library(tidyverse)

path <- "~/Dropbox/Thesis/inc_adv/data/federal"

readin_files <- function (files, year) {
  data <- read_csv(paste0(path, "/", year, "/", files), skip = 1)
  return(data)
}

# YEAR 2019
all_2019 <- list.files(file.path(path, "2019"))

data_2019 <- sapply(all_2019, readin_files, c(2019))




# candidate-level data
fp_cand_2019 <- read_csv(file.path(path, "/2019/2019_fp_cand_federal.csv"), skip = 1)


glimpse(fp_cand_2019)
