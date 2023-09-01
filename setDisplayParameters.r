# Function to set gtsummary display parameters.
setDisplayParameters <- function(
    percent = FALSE # = TRUE to append "%" to displayed values for continuous outcomes
) {
    
    ifelse(
        percent == FALSE,
        {
            # statistic_all_continuous <- c("{N_nonmiss} ({p_nonmiss})", "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}", "{N_miss} ({p_miss})"),
            statistic_all_continuous <- c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}", "{N_miss} ({p_miss})")
        },
        {
            # statistic_all_continuous <- c("{N_nonmiss} ({p_nonmiss})", "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}", "{N_miss} ({p_miss})")
            statistic_all_continuous <- c("{mean}% ({sd}%)", "{median}% ({p25}%, {p75}%)", "{min}%, {max}%", "{N_miss} ({p_miss})")
        }
    )
    statistic_all_categorical <- c("{n} ({p})")
        
    # digits_all_continuous <- c(c(0, 1), c(1, 2), c(1, 0, 0), c(0, 0), c(0, 1))
    digits_all_continuous <- c(c(1, 2), c(1, 0, 0), c(0, 0), c(0, 1))
    digits_all_categorical <- c(0, 1)

    # label_all_continuous <- c("N (%)", "Mean (SD)", "Median (IQR)", "Min, Max", "Missing (n (%))")
    label_all_continuous <- c("Mean (SD)", "Median (IQR)", "Min, Max", "Missing (n (%))")
    label_all_categorical <- "N (%)"
    
    list(
        
        statistic_all_continuous = statistic_all_continuous,
        statistic_all_categorical = statistic_all_categorical,
        digits_all_continuous = digits_all_continuous,
        digits_all_categorical = digits_all_categorical,
        label_all_continuous = label_all_continuous,
        label_all_categorical = label_all_categorical
        
    )
    
}