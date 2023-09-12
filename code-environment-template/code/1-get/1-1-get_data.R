# Project: 
#   Links: 
#   Description: 
# 
# Data flow
#   See config.json for data folder (i.e., config['path_data']).
#   Input: files in config['input*'] folders from config.json.
#   Output: files in config['output'] and config['output_pd'] folders from config.json.



# Preliminary processing
# RUN THIS CODE IN TERMINAL BEFORE RUNNING THIS CELL TO PREVENT INSTALL.PACKAGES() ERRORS:
# sudo chmod -R 777 /opt/domino/spark/R/


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


# ## Load custom tools.
# path_ejdtools <- "/.../ejdtools/"
# files_sources <- list.files(path_ejdtools)
# files_sources <- files_sources[grepl(".R", files_sources)]
# sapply(paste0(path_ejdtools, files_sources), source)
# rm(files_sources)



# Gather data.


## Dataset 1
[temp_name1]_[1] <- ...
...
[temp_name1]_[m] <- ...

### Output data.

[final_name1] <- [temp_name1]_[m]

[final_name1] %>% str
feather::write_feather([final_name1], paste0(config$output_pd, "[final_name1].feather"))


## Dataset n

### Output data.