# Calculation Characteristics:
#   mean known
#   SD known
#   ICC known or unknown
# 
# Project: [insert project name]
#   Links: [insert project folder link]
# 
#   Description: Power and sample size justification for common requests with vague research
#     hypotheses involving comparisons (e.g., one-sample, two-sample) and possible multiple
#     testing (i.e., multiplicity).



# Preliminary processing


## Clear environment and install/load packages.
rm(list=ls())
if (!require("pacman")) install.packages("pacman") # just to make sure "pacman" is installed
pacman::p_load(tidyverse, knitr, ggpubr) # e.g., naniar, extrafont, tidyverse, reshape2, janitor, lubridate, jsonlite, arsenal, knitr, feather


## Load custom tools.
utils <- "/Users/ericjaydaza/Documents/Github/[project_codename]/code/0-utils/"
files_sources <- list.files(utils)
files_sources <- files_sources[grepl(".r", tolower(files_sources))]
sapply(paste0(utils, files_sources), source) %>% invisible()
rm(utils, files_sources)


## Read config file. The part of the path before "output/config.json"
## should exactly match config$path_data.
config <- jsonlite::fromJSON("/Users/ericjaydaza/Documents/Github/[project_codename]/data/output/config.json")
str(config)


## Set global parameters.
scalar_seed <- # optional: specify random number seed for reproducibility
output_path <- config$path_data # your local path and project folder within the "data" folder

### PSS parameters
delta_primary <- 10 # mean of primary/main objective effect or association size; include literature reference (link if possible) in this comment
sd_primary <-  # standard deviation of primary/main objective effect or association; include literature reference (link if possible) in this comment

sd_secondary <- # optional
delta_secondary <- # optional



# Simulate data.


## Set parameters.

### Calculate correct SD of the difference between correlated pre-post outcomes
### per participant.
### 
### https://en.wikipedia.org/wiki/Effect_size#Cohen's_d
### https://en.wikipedia.org/wiki/Student%27s_t-test
### https://en.wikipedia.org/wiki/Pooled_variance

### We consider sd_primary to be equal between the treatment and control groups. Hence,
### the pooled SD is just sd_primary, and thus Cohen's d = delta_primary / sd_primary.
cohens_d_initial <- delta_primary / sd_primary # "initial" is appended in case we optionally calculate new Cohen's d values affected by ICCs (see below)

### anticipated proportion lost to follow-up or attrition
p_attrition <- 0.40
# p_attrition <- 0.35 # gets n=50 per arm under initial parameters

### number of statistical hypothesis tests for all relevant primary (and maybe secondary) objectives
num_of_tests <- 2

### statistical significance (discernibility) thresholds
# alphas_familywise <- c(0.05, 0.01, 0.001)
alphas_familywise <- c(0.10, 0.05, 0.01)

### statistical power levels
# power_levels <- c(0.80, 0.90, 0.99)
power_levels <- c(0.80, 0.90)

# ### optional: intraclass correlations (ICCs) for pre-post outcomes / change scores
# ### (i.e., per-participant outcome is a change from baseline to follow-up)
# chosen_icc <- 0.79 # ICC to use; include literature reference (link if possible) in this comment
# iccs <- c(0.5, 0.7, chosen_icc)

### export parameters
dplyr::tibble(
  
  delta_primary = delta_primary,
  delta_secondary = delta_secondary,
  sd_primary = sd_primary,
  sd_secondary = sd_secondary,
  cohens_d_initial = cohens_d_initial,
  num_of_tests = num_of_tests,
  p_attrition = p_attrition
  # , chosen_icc = chosen_icc # optional
  
) %>% readr::write_csv(paste0(output_path, "tbl_pss_parameters.csv"))

### types of tests

# types_of_test <- c("one.sample", "two.sample")
types_of_test <- c("paired", "one.sample", "two.sample")

# default version if not using ICCs; comment out if using ICCs
type_of_test <- c(
  rep(types_of_test[1], length(alphas_familywise) * length(power_levels)),
  rep(types_of_test[2], length(alphas_familywise) * length(power_levels)),
  rep(types_of_test[3], length(alphas_familywise) * length(power_levels))
)

# # optional version if using ICCs; comment out if not using ICCs
# type_of_test <- c(
#   rep(types_of_test[1], length(alphas_familywise) * length(power_levels) * length(iccs)),
#   rep(types_of_test[2], length(alphas_familywise) * length(power_levels) * length(iccs)),
#   rep(types_of_test[3], length(alphas_familywise) * length(power_levels) * length(iccs))
# )


## Simulate scenarios by discernibility and power levels.
tbl_sims_1 <- dplyr::tibble(
  
  # default version if not using ICCs; comment out if using ICCs
  alpha_familywise =  rep(1, length(types_of_test)) %x% alphas_familywise %x% rep(1, length(power_levels)) %>% as.numeric(),
  alpha_per_test = alpha_familywise %>% as.numeric() / num_of_tests,
  power_level = rep(1, length(types_of_test)) %x% rep(1, length(alphas_familywise)) %x% power_levels %>% as.numeric(),
  delta_primary = delta_primary,
  sd_primary = sd_primary,
  cohens_d = delta_primary / sd_primary,
  type_of_test = type_of_test
  
  # # optional version if using ICCs; comment out if not using ICCs
  # alpha_familywise =  rep(1, length(types_of_test)) %x% alphas_familywise %x% rep(1, length(power_levels)) %x% rep(1, length(iccs)) %>% as.numeric(),
  # alpha_per_test = alpha_familywise %>% as.numeric() / num_of_tests,
  # power_level = rep(1, length(types_of_test)) %x% rep(1, length(alphas_familywise)) %x% power_levels %x% rep(1, length(iccs)) %>% as.numeric(),
  # icc = rep(1, length(types_of_test)) %x% rep(1, length(alphas_familywise)) %x% rep(1, length(power_levels)) %x% iccs %>% as.numeric(),
  # delta_primary = delta_primary,
  # sd_primary = sd_primary,
  # cohens_d = delta_primary / sqrt(2 * sd_primary^2 * (1-icc)),
  # type_of_test = type_of_test
  
)
tbl_sims_1$sampsize <- apply(
  X = tbl_sims_1,
  MARGIN = 1,
  # FUN = function(x) pwr::pwr.t.test(
  #   d = x["cohens_d"] %>% as.numeric,
  FUN = function(x) stats::power.t.test(
    delta = x["delta_primary"] %>% as.numeric,
    sd = x["sd_primary"] %>% as.numeric,
    power = x["power_level"] %>% as.numeric,
    type = x["type_of_test"],
    alternative = "two.sided"
  )$n %>% ceiling()
)
tbl_sims_2 <- tbl_sims_1 %>%
  dplyr::mutate(sampsize_attrition = ceiling(sampsize / (1 - p_attrition))) %>%
  dplyr::group_by(type_of_test) %>%
  dplyr::mutate(Scenario = dplyr::row_number()) %>%
  dplyr::ungroup()

tbl_sims_final <- tbl_sims_2

### Export.
tbl_sims_final %>% readr::write_csv(paste0(output_path, "tbl_sims_final_ntests", num_of_tests, ".csv"))


## Make table with fancy headers
tbl_sims_fancy <- tbl_sims_final %>%
  dplyr::mutate(
    
    type_of_test = dplyr::case_when(
      
      type_of_test == "paired" ~ "paired",
      type_of_test == "one.sample" ~ "one-sample",
      type_of_test == "two.sample" ~ "two-sample"
      
    ),
    delta_primary = delta_primary %>% round(3),
    sd_primary = sd_primary %>% round(3),
    cohens_d = cohens_d %>% round(3)
    
  ) %>%
  dplyr::select( # re-order columns
    type_of_test,
    Scenario,
    alpha_familywise,
    alpha_per_test,
    power_level,
    # icc, # optional
    delta_primary,
    sd_primary,
    cohens_d,
    sampsize,
    sampsize_attrition
  ) %>%
  dplyr::rename(
    `Type of Test` = type_of_test,
    `Fam. FPR` = alpha_familywise,
    `Per-test FPR` = alpha_per_test,
    Power = power_level,
    # ICC = icc, # optional
    Delta = delta_primary,
    SD = sd_primary,
    `Cohen's d` = cohens_d,
    `Sample Size` = sampsize,
    `Samp. Size (Att.-Adj.)` = sampsize_attrition
  )

### Export.
tbl_sims_fancy %>% readr::write_csv(paste0(output_path, "tbl_sims_fancy_ntests", num_of_tests, ".csv"))


## Make table with sentences.
tbl_sims_sentences <- tbl_sims_final %>%
  dplyr::mutate(
    
    string_type_of_test = ifelse(
      type_of_test %in% c("paired", "one.sample"),
      "difference from zero",
      "difference between two groups"
    ),
    Description = paste0(
      "With ",
      (1 - alpha_per_test) * 100,
      "% confidence (corresponding to a max false-positive rate of ",
      # alpha_per_test / num_of_tests, # this seems to have been incorrect from 2 Oct 2021
      alpha_per_test,
      " for each of ",
      num_of_tests,
      " tests), enrolling ",
      sampsize_attrition,
      ifelse(
        type_of_test %in% c("paired", "one.sample"),
        " participants (with at most ",
        " participants in each treatment arm (with at most "
      ),
      p_attrition * 100,
      "% attrition down to ",
      sampsize,
      " participants) provides at least ",
      power_level * 100, "%",
      " power to discern or detect a true average ",
      string_type_of_test,
      " of at least ",
      round(delta_primary, 2),
      " units or greater, with an outcome SD of ",
      round(sd_primary, 2),
      # " and an intraclass correlation (ICC) of ", # optional
      # round(icc, 2), # optional
      "."
      # " at least as large as the true hypothesized average difference."
    )
    
  ) %>%
  dplyr::select(type_of_test, Scenario, Description)

### Export.
tbl_sims_sentences %>% readr::write_csv(paste0(output_path, "tbl_sims_sentences_ntests", num_of_tests, ".csv"))



# Output data.


## Save data image for loading in report .Rmd.
save.image(paste0(output_path, "pss_image.Rdata"))