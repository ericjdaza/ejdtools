# Project: 
#   Links: 
#   Description: 
# 
# Data flow
#   Input: none.
#   Output: config.json located in output key specified in config.json specified below.



# Preliminary processing
# RUN THIS CODE IN TERMINAL BEFORE RUNNING THIS CELL TO PREVENT INSTALL.PACKAGES() ERRORS:
# sudo chmod -R 777 /opt/domino/spark/R/


## Install/load packages.
if (!require("pacman")) install.packages("pacman") # just to make sure "pacman" is installed
pacman::p_load(jsonlite)



# Create config file.
path_code <- # e.g., "/.../[project_slug]/code/"
path_data <- # e.g., "/.../[project_slug]/data/"
config <- jsonlite::write_json( # https://rdrr.io/github/skgrange/threadr/man/write_json.html
  
  list(
    
    path_code =   path_code,
    path_data =   path_data,
    
    input =       paste0(path_data, "input/"),

    output =      paste0(path_data, "output/"),
    output_pd =   paste0(path_data, "output/0-processed-data/"),
    output_ad =   paste0(path_data, "output/1-analysis-data/"),
    output_ao =   paste0(path_data, "output/2-analysis-output/")
    
  ),
  paste0(path_data, "output/config.json")
  
)
rm(path_code, path_data)