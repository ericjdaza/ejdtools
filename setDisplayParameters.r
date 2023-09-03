# Function to set gtsummary display parameters.
setDisplayParameters <- function(
    add_n_obs = FALSE, # set to TRUE to append total number of observations
    percent = FALSE # set to TRUE to append "%"
) {
    
    ifelse(
        percent == FALSE,
        {
            # statistic_all_continuous <- c(
            #     "{N_nonmiss} ({p_nonmiss})",
            #     "{mean} ({sd})",
            #     "{median} ({p25}, {p75})",
            #     "{min}, {max}",
            #     "{N_miss} ({p_miss})"
            # ),
            statistic_all_continuous <- c(
                "{mean} ({sd})",
                "{median} ({p25}, {p75})",
                "{min}, {max}",
                "{N_miss} ({p_miss})"
            )            
            statistic_all_categorical <- c("{n} ({p})")
        },
        {
            # statistic_all_continuous <- c(
            #     "{N_nonmiss} ({p_nonmiss})",
            #     "{mean} ({sd})",
            #     "{median} ({p25}, {p75})",
            #     "{min}, {max}",
            #     "{N_miss} ({p_miss})"
            # )
            statistic_all_continuous <- c(
                "{mean}% ({sd}%)",
                "{median}% ({p25}%, {p75}%)",
                "{min}%, {max}%",
                "{N_miss} ({p_miss})"
            )
            statistic_all_categorical <- c("{n} ({p}%)")
        }
    )
    if (add_n_obs == TRUE) {
        
        statistic_all_continuous <- c(
            "{N_obs}",
            statistic_all_continuous
        )
        statistic_all_categorical <- c("{n} ({p}%) (N={N})")
        
    }
    # statistic_list <- list(
    #     all_continuous() ~ statistic_all_continuous,
    #     all_categorical() ~ statistic_all_categorical
    # )
    
    # digits_all_continuous <- c(
    #     c(0, 1),
    #     c(1, 2),
    #     c(1, 0, 0),
    #     c(0, 0),
    #     c(0, 1)
    # )
    digits_all_continuous <- c(
        c(1, 2),
        c(1, 0, 0),
        c(0, 0),
        c(0, 1)
    )
    digits_all_categorical <- c(0, 1)
    if (add_n_obs == TRUE) {
        
        digits_all_continuous <- c(
            0,
            digits_all_continuous
        )
        digits_all_categorical <- c(0, 1, 0)
        
    }
    # digits_list <- list(
    #     all_continuous() ~ digits_all_continuous,
    #     all_categorical() ~ digits_all_categorical
    # )

    # label_all_continuous <- c(
    #     "n (%)",
    #     "Mean (SD)",
    #     "Median (IQR)",
    #     "Min, Max",
    #     "Missing (n (%))"
    # )
    label_all_continuous <- c(
        "Mean (SD)",
        "Median (IQR)",
        "Min, Max",
        "Missing (n (%))"
    )
    label_all_categorical <- c("n (%)")
    if (add_n_obs == TRUE) {
        
        label_all_continuous <- c(
            "N",
            label_all_continuous
        )
        # label_all_categorical <- c("n (%) (N)")
        
    }
    # label_list <- list(
    #     all_continuous() ~ label_all_continuous,
    #     all_categorical() ~ label_all_categorical
    # )
    
    list(
        
        statistic_all_continuous = statistic_all_continuous,
        statistic_all_categorical = statistic_all_categorical,
        # statistic_list,
        
        digits_all_continuous = digits_all_continuous,
        digits_all_categorical = digits_all_categorical,
        # digits_list = digits_list,
        
        label_all_continuous = label_all_continuous,
        label_all_categorical = label_all_categorical
        # , label_list = label_list
        
    )
    
}