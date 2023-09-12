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
    
    path_code = path_code,
    path_data = path_data,
    
    input = paste0(path_data, "input/"),
    
    output = paste0(path_data, "output/"),
    got = paste0(path_data, "output/1-got/"),
    processed = paste0(path_data, "output/2-processed/"),
    analyzed = paste0(path_data, "output/3-analyzed/")
    
    # # example outputs:
    # got_file1 = "file1.csv",
    # got_file2 = "file2.feather",
    # processed_file1 = "file1.feather",
    # analyzed_file1 = "file1.rtf",
    # analyzed_file2 = "file2.png"
    
  ),
  paste0(path_data, "output/config.json")
  
)
rm(path_code, path_data)