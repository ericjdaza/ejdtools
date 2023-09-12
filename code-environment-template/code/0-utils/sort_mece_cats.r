# +
# Function to sort mutually exclusive, completely exhaustive (MECE) categorical variables as desired.
# For example, a multi-select question ("select all that apply") is not MECE.
# For proper processing, do this based on only observed values, not all possible values.
sort_mece_cats <- function(
    data, # input dataset
    varname, # MECE variable with values to sort
    list_pairs, # make_and_order_value_label_pairs() output list (e.g., "pairs_gender") for relevant variable to summarize
    add_missing_level = FALSE, # set to TRUE to add "missing" level to output if NA is actually observed; not commonly used
        missing_level = "Missing" # only applies if add_missing_level = TRUE and NA is actually observed
) {
    
    varvals_observed <- unique(data[[varname]])
    for (valuename in names(list_pairs)) {
        
        ifelse(
            valuename == names(list_pairs)[1],
            varvals_levels <- ifelse(valuename %in% varvals_observed, valuename, NA),
            varvals_levels <- c(
                varvals_levels,
                ifelse(valuename %in% varvals_observed, valuename, NA)
            )
        )
        
    }
    varvals_levels <- varvals_levels %>% na.omit()
    
    # if desired, add in a "missing" level to account for missing values
    if (add_missing_level == TRUE & NA %in% varvals_observed) varvals_levels <- c(
        varvals_levels,
        missing_level
    )
    
    return(
        list(
            varvals_observed = varvals_observed,
            varvals_levels = varvals_levels
        )
    )

}
