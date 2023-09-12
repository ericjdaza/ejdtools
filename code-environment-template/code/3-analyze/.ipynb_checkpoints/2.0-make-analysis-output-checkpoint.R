# Project: 
#   Jira: 
# 
# Data flow
#   See config.json for data folder (i.e., config['path_data']).
#   Input: files in config$['nput*'], config['output'], config['output_pd'], and config['output_ad'] folders from config.json.
#   Output: files in config['output'] and config['output_ao'] folders from config.json.



# Preliminary processing


## Clear environment and install/load packages.
rm(list=ls())
if (!require("pacman")) install.packages("pacman") # just to make sure "pacman" is installed
pacman::p_load() # e.g., naniar, extrafont, tidyverse, reshape2, janitor, lubridate, jsonlite, arsenal, knitr, feather


## Set global parameters.
scalar_seed <- 
  
  
  ## Read config file. The part of the path before "output/config.json"
  ## should exactly match config$path_data.
  config <- jsonlite::fromJSON("/.../output/config.json")
str(config)


## Load custom tools.
files_sources <- list.files(paste0(path_code, "ejdtools/"))
sapply(paste0(path_code, "ejdtools/", files_sources), source)
rm(files_sources)



# Gather data.


## Dataset 1
[temp_name1]_[1] <- ...
...
[temp_name1]_[m] <- ...

### Output data.

[final_name1] <- [temp_name1]_[m]

[final_name1] %>% str
feather::write_feather([final_name1], paste0(config$output_ao, "/[final_name1].feather"))


## Dataset n

### Output data.



# Process data.

### Output data.



# Analyze data.

### Output data.