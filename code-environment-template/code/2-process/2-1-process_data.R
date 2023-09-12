# Preliminary processing
# RUN THIS CODE IN TERMINAL BEFORE RUNNING THIS CELL TO PREVENT INSTALL.PACKAGES() ERRORS:
# sudo chmod -R 777 /opt/domino/spark/R/


## Clear environment and install/load packages.
rm(list=ls())
if (!require("pacman")) install.packages("pacman") # just to make sure "pacman" is installed
pacman::p_load(tidyverse) # e.g., naniar, extrafont, tidyverse, reshape2, janitor, lubridate, jsonlite, arsenal, knitr, feather


## Load custom tools.
utils <- "/.../[project_name]/code/0-utils/"
files_sources <- list.files(utils)
files_sources <- files_sources[grepl(".r", tolower(files_sources))]
sapply(paste0(utils, files_sources), source) %>% invisible()
rm(utils, files_sources)


## Set global parameters.
# scalar_seed <- 


## Read config file. The part of the path before "output/config.json"
## should exactly match config$path_data.
config <- jsonlite::fromJSON("/.../output/config.json")
str(config)



# Get data.

# Process data.

# Export.