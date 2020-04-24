library(tidyverse)
library(readtext)

#####################################################
####### Cleaning text data #########
# read all text files
# extract:
# - names of the winners 
# - winning % Of the TPP
# - the electoral constituency
#####################################################

path <- "/Users/anniechen/Documents/GitHub/annie_better_document_your_stuff/other_scripts"

#list.files(path, pattern = ".txt")

data <- readtext(paste0(path, "/out.txt"))

# extract name before the astericks
str_extract_all(string = data$text, pattern = ".\\*")
