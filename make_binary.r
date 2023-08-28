# +
# Function to transform variables to binary indicators. Adds a "missing" category if all binary indicators are 0 or FALSE.
make_binary <- function(
    data, # input dataset
    list_pairs # make_and_order_value_label_pairs() output list (e.g., "pairs_gender") for relevant variable to summarize
) {
    
    tbl_out <- data
    
    for (valuename in names(list_pairs)) tbl_out[[valuename]] <- !is.na(tbl_out[[valuename]]) # use this for FALSE and TRUE values
#     for (valuename in names(list_pairs)) tbl_out[[valuename]] <- as.numeric(!is.na(tbl_out[[valuename]])) # use this for 0 and 1 values
    rm(valuename)
    
    tbl_all_missing <- tbl_out %>%
        dplyr::select({names(list_pairs)})
    tbl_all_missing$all_missing <- rowSums(tbl_all_missing)
    for (valuename in names(list_pairs)) tbl_out[[valuename]][tbl_all_missing$all_missing == 0] <- NA
    
    return(tbl_out)

}
