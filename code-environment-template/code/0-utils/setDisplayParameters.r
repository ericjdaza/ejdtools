# Function to set gtsummary display parameters.
setDisplayParameters <- function(
    add_n_obs = FALSE, # set to TRUE to append total number of observations
    percent = FALSE, # set to TRUE to append "%" for continuous variables that represent percentages
    percent_categorical = TRUE, # set to TRUE to append "%" to "{p}" for categorical variables
    footnote_non_mece = "\\>1 response allowed per participant for the following variables (i.e., percentages may sum to >100%): "
) {
    
    statistic_all_continuous <- c(
        "{mean} ({sd})",
        "{median} ({p25}, {p75})",
        "{min}, {max}"
        # "{N_miss} ({p_miss})"
    )            
    if (percent == TRUE) statistic_all_continuous <- c(
        "{mean}% ({sd}%)",
        "{median}% ({p25}%, {p75}%)",
        "{min}%, {max}%"
        # "{N_miss} ({p_miss})"
    )
    statistic_all_categorical <- c("{n} ({p})")
    if (percent_categorical == TRUE) statistic_all_categorical <- c("{n} ({p}%)")
    if (add_n_obs == TRUE) {
        
        statistic_all_continuous <- c(
            "{N_obs}",
            statistic_all_continuous
        )
        statistic_all_categorical <- c("{n} ({p}%) (N={N})")
        
    }
    
#     digits_all_continuous <- c(
#         c(1, 2),
#         c(1, 0, 0),
#         c(0, 0)
#         # c(0, 1)
#     )
#     digits_all_categorical <- c(0, 1)
#     if (add_n_obs == TRUE) {
        
#         digits_all_continuous <- c(
#             0,
#             digits_all_continuous
#         )
#         digits_all_categorical <- c(0, 1, 0)
        
#     }
    
    # Have gtsummary automatically determine digits by setting
    # these to NULL.
    digits_all_continuous <- NULL
    digits_all_categorical <- NULL
    
    label_all_continuous <- c(
        "Mean (SD)",
        "Median (IQR)",
        "Min, Max"
        # "Missing (n (%))"
    )
    label_all_categorical <- c("n (%)")
    if (add_n_obs == TRUE) {
        
        label_all_continuous <- c(
            "N",
            label_all_continuous
        )
        
    }
    
    list(
        
        statistic_all_continuous = statistic_all_continuous,
        statistic_all_categorical = statistic_all_categorical,
        
        digits_all_continuous = digits_all_continuous,
        digits_all_categorical = digits_all_categorical,
        
        label_all_continuous = label_all_continuous,
        label_all_categorical = label_all_categorical,
        
        footnote_non_mece = footnote_non_mece
        
    )
    
}