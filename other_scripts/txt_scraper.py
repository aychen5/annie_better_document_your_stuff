import numpy as np
import os
import sys
import re
import pandas as pd
import requests
import urllib3
from bs4 import BeautifulSoup
#import nltk

URL = "http://psephos.adam-carr.net/countries/a/australia/1998/1998repsnsw.txt"

response = requests.get(URL)

#BeautifulSoup object into string
data = BeautifulSoup(response.content, "html.parser")
html = data.prettify()

# write as text file
with open("out.txt","w") as out:
    for i in range(0, len(html)):
        try:
            out.write(html[i])
        except Exception:
            1+1


# opening text file
my_file = open('out.txt').read()
list_strings = my_file.split('\n')

# need to search for strings where there is an asterisk beside the candidate name
for i in range(len(list_strings)):
   print(re.search(r"^[a-z]+[*]?$", list_strings[i]))

# why is this not matching??
re.search("[a-z]+\*?$", list_strings[7]) == None
