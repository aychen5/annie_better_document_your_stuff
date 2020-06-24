### Description
Documents for replicability (and so I can recover things I accidentally delete =/ᐠ｡‸｡ᐟ\\=∫).

### Organization

<pre>
. 
├── README.md 
├── /raw_data (in Dropbox)
│   ├── /federal
│       ├── /lower
│           ├── /year
│           └── contains: list of candidates, tcp, tpp, and two fp .csvs (party-level and candidate-level)
│       └── /upper
│           └── fp_senate_data (2004 - 2019)
│   ├── /state
│           └── /year
│   ├── /census
│   └── /surveys
│           └── AES data (1998 - 2019)
├── /clean_data (in Dropbox)
│   ├── /year 
│       └── contains: cleaned .csvs for each state and year 1949 to 2001
│   ├── tpp_data.csv
│   ├── tcp_data.csv
│   ├── fp_data.csv
│   └── mrp.csv
├── /main
│   ├── /inc_adv 
│   └── contains: .Rproject, scripts for cleaning and general analysis
│       ├── cleaning
│           ├── cleaning_data.R 
│           ├── cleaning_old_data.R 
│           ├── cleaning_state_data.R 
│           ├── parse_australia_txt.R (clean data frames from text data for election results 1998 and earlier)
│           ├── fourth_line_divs.R (function to deal with formatting anomalies in txt data)
│           ├── log_manual.R (some manual re-coding is inevitable...)
│       ├── analysis
│           ├── sorting_simulation.R 
│           ├── RDD_state.R
│           ├── RDD_party_federal.R
│           └── RDD_candidate_federal.R
│       ├── descriptives
│           ├── dag.R 
│           ├── descriptives.R 
│           ├── mrp.R 
│           └── senate_data.R 
│       └── tests
│           ├── covariates.R 
├── /other_scripts
│   └── contains: misc code (e.g. for scraping)
│       └── txt_scraper.py (to get Adam Carr's text data)
│       └── /bounds
│           └── rdbound.ado
├── /extra
│   ├── /campaign_finance
│   └── /old_code
│       └── fix.Rmd
│       └── tests.Rmd
.    
</pre>

    
