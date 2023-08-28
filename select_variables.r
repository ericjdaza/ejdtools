# +
# Function to select variables for formatting.
select_variables <- function(
    data, # input dataset
    period_variable = "week_of_study", # e.g., "week_of_study", "period_of_day"
    vars_to_summarize,
    grouping_variable = "device_worn" # e.g., cohort, device, device_worn
){
    
    data %>%
        dplyr::select(

            # individual identifier
            identifier,

            # time interval
            {period_variable},

            # variables to summarize
            {vars_to_summarize},

            # grouping variable
            {grouping_variable}

        )

}
