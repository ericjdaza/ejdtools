# Function to calculate study day or week.
calculateStudyDayOrWeek <- function(
    data,
    start_date_variable,
    end_date_variable,
    new_variable_name,
    last_planned_timepoint = NA,
    past_last_planned_timepoint = NA,
    difftime_units = "days" # "days", "weeks"; see https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/difftime
) {
    
    output <- data
    
    names(output)[names(output) == start_date_variable] <- "start_date_variable"
    names(output)[names(output) == end_date_variable] <- "end_date_variable"
    
    output <- output %>%
        dplyr::mutate(
            
            numeric_difftime_value = difftime(
                end_date_variable,
                start_date_variable,
                units = difftime_units
            ) %>% as.numeric(),
            new_variable_name = ifelse(
                numeric_difftime_value >= 0,
                numeric_difftime_value + 1,
                NA
            )
            
        )
    
    if (!is.na(last_planned_timepoint)) output <- output %>%
        dplyr::mutate(
            
            past_last_planned_timepoint = (new_variable_name > last_planned_timepoint),
            time_point_past_last_planned_timepoint = ifelse(
                past_last_planned_timepoint == TRUE,
                new_variable_name - last_planned_timepoint,
                NA
            )
            
        )
    
    names(output)[names(output) == "start_date_variable"] <- start_date_variable
    names(output)[names(output) == "end_date_variable"] <- end_date_variable
    names(output)[names(output) == "new_variable_name"] <- new_variable_name
    if (!is.na(last_planned_timepoint)) {
        
        names(output)[names(output) == "time_point_past_last_planned_timepoint"] <- paste0(
            difftime_units,
            "_past_last_planned_",
            new_variable_name
        )
        if (!is.na(past_last_planned_timepoint))
            names(output)[names(output) == "past_last_planned_timepoint"] <- past_last_planned_timepoint
        
    }
    if (!is.na(past_last_planned_timepoint))
        names(output)[names(output) == "past_last_planned_timepoint"] <- past_last_planned_timepoint
    
    return(output)
    
}