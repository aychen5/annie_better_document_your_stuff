### Description
Documents for replicability (and so I can recover things I accidentally delete =/ᐠ｡‸｡ᐟ\\=∫).

### Organization

<pre>
. 
├── README.md 
├── /data (in Dropbox)
│   ├── /federal
│       ├── /lower
│           ├── /year
│           └── contains: list of candidates, tcp, tpp, and two fp .csvs (party-level and candidate-level)
│       └── /senate
│           └── /year
│   └── /state
│           └── /year
│   └── /local
│           └── /year
├── /clean_data (in Dropbox)
│   ├── tpp_data.csv
├── /main_analysis
│   ├── /inc_adv 
│   └── contains: .Rproject, scripts for cleaning and general analysis
│       ├── cleaning
│           ├── cleaning_data.R 
│           ├── parse_australia_txt.R (clean data frames from text data for election results 1998 and earlier)
│           ├── fourth_line_divs.R (function to deal with formatting anomalies in txt data)
│           ├── log_manual.R (some manual re-coding is inevitable...)
│       ├── analysis.R
│           ├── RDD_party_federal.R
│           └── RDD_candidate_federal.R
│       └── sorting_simulation.R 
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

    
