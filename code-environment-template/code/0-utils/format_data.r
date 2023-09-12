# +
# Function to format data for processing by gtsummary.
format_data <- function(
    data, # input dataset
    period_variable = "week_of_study", # e.g., "week_of_study", "period_of_day"
    period_variable_label = "Week", # e.g., "Week", "Time Period"
    vars_to_tabulate,
    grouping_variable = "device_worn" # e.g., cohort, device, device_worn
){
    
    # Pivot to cast weeks as separate columns. This is needed for gtsummary to
    # display concatenated weekly statistics.
    tbl_out <- data %>%
        tidyr::pivot_wider(

            id_cols = c(identifier, {grouping_variable}),
            names_from = {period_variable},
            values_from = {vars_to_tabulate},
            names_prefix = period_variable_label

        ) %>%
        dplyr::arrange(identifier) %>%
        dplyr::select({grouping_variable}, dplyr::starts_with(period_variable_label))
    
    # Add a space after the variable name prefix "Week"; e.g., change "Week1" to "Week 1".
    names(tbl_out) <- stringr::str_replace(names(tbl_out), period_variable_label, paste0(period_variable_label, " "))
    
    return(tbl_out)

}
