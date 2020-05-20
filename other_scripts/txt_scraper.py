import numpy as np
import os
import sys
import re
import pandas as pd
import requests
import urllib3
from bs4 import BeautifulSoup
#import nltk


### ------- get all urls for a given legislative election ------- ###

# try 1990, 1993, and 1996
#years = list(range(1990, 1998))[::3]

# legislative elections are not always 3 years apart...
years = [1987, 1984, 1983, 1980, 1977, 1975, 1972, 1966, 1963, 1961, 1958, 1955, 1954, 1951, 1949]

def get_urls(election_year):
    """ This function takes as input the election year and outputs
    the corresponding URLs from Adam Carr's database of Australian
    election results for each Australian State. The data are at the
    constituency level.
    """
    base = "http://psephos.adam-carr.net/countries/a/australia/"
    # there are 8 states
    states = ['nsw', 'vic', 'qld', 'wa', 'sa', 'tas', 'act', 'nt']
    # use f-strings to construct file names
    urls = list()
    for x in range(len(states)):
        txt_file = f"{election_year}{'reps'}{states[x]}{'.txt'}"
        urls.append(f"{base}{election_year}{'/'}{txt_file}")

    return urls

URLs = list()
for i in range(len(years)):
    URLs.append(get_urls(years[i]))

#URLs

### ------- extract text file from URL ------- ###

# use list comprehension...more pythonic, but less readable with nested lists
#for i in range(len(URLs[i])):
#    responses.append(list(map(requests.get, URLs[i])))
responses = [[requests.get(i) for i in URLs[i]] for URLs[i] in URLs]

# check that data look okay
def response_parser(responses):
    data = list()
    for x in range(len(responses)):
        for y in range(len(responses[x])):
            data.append(BeautifulSoup(responses[x][y].content, "html.parser"))
    return(data)

data = response_parser(responses)

#len(data)

### ------- save as text file ------- ###

# 0 = 1990, 1 = 1993, 2 = 1996
# 0 = nsw, 1 = vic,..., 7 = nt

def save_txt_file(save_path, year = list(range(1990, 1998))[::3]):
    """ This function takes the response urls and saves them as text files
    in the appropriate Dropbox folder with the proper labels. You must
    specify a file path and a list of years.
    """
    path = save_path
    states = ['nsw', 'vic', 'qld', 'wa', 'sa', 'tas', 'act', 'nt']
    years = year
    for y in range(len(responses)):
        for s in range(len(responses[y])):
            r = responses[y][s]
            state = states[s]
            year = years[y]
            file_name = f"{'fp_'}{state}{'_'}{year}{'.txt'}"
            with open(f"{path}{'/'}{year}{'/'}{file_name}", 'w') as file:
                file.write(r.text)

save_txt_file(save_path = "/Users/anniechen/Dropbox/Thesis/inc_adv/data/federal/lower/", year = years)


### -------  migrate tidying of data to R ------- ###

### opening text file
my_file = open('out.txt').read()
list_strings = my_file.split('\n')

# need to search for strings where there is an asterisk beside the candidate name
for i in range(len(list_strings)):
   print(re.search(r"^[a-z]+[*]?$", list_strings[i]))
