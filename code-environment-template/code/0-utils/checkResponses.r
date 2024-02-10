# Function to check mismatched responses at same time point.

## subfunction 1
checkResponses_filter <- function(
    data,
    survey_type_value, # "baseline_survey", "weekly_survey", "final_survey"
    created_at_date_value,
    modified_at_date_value,
    number_of_responses_value # number of responses per question_id, question_text, and modified_at_date
) {
    
    output <- data
    
    # Filter for survey_type.
    output <- output %>%
        dplyr::filter(survey_type == survey_type_value)
    
    # Filter for created_at_date_value
    output <- output %>%
        dplyr::filter(created_at_date == lubridate::as_date(created_at_date_value))
    
    # Filter for modified_at_date_value.
    output <- output %>%
        dplyr::filter(modified_at_date == lubridate::as_date(modified_at_date_value))
    
    # Filter for number_of_responses.
    output <- output %>%
        dplyr::filter(number_of_responses == number_of_responses_value)
    
    return(output)

}

## subfunction 2
checkResponses_pivot <- function(data) {
    
    output <- data
    
    # Concatenate question_id and question_text for correct pivoting.
    output <- output %>%
        dplyr::mutate(

            concatenated = paste0(
                "question_id_",
                question_id,
                "__question_text_",
                question_text
            )

        ) %>%
        dplyr::select(-c(

            question_id,
            question_text

        ))
    
    # Pivot.
    output <- output %>%
        tidyr::pivot_wider(
            names_from = concatenated,
            values_from = answer_text
        )
    
    # Sort by modified_at_time.
    output <- output %>%
        dplyr::arrange(modified_at_time)
    
    return(output)

}

## subfunction 3
checkResponses_transpose_filterMismatches <- function(
    data,
    # survey_type_value, # "baseline_survey", "weekly_survey", "final_survey"
    # modified_at_date_value,
    number_of_responses_value # number of responses per question_id, question_text, and modified_at_date
) {
    
    output <- data
    
    # Transpose.
    output <- output %>%
        t()
    
    # Filter for rows with mismatched responses.
    if (number_of_responses_value == 2) output <- output[
        (output[,1] != output[,2]),
    ]
    if (number_of_responses_value == 3) output <- output[
        (output[,1] != output[,2]) |
        (output[,1] != output[,3]) |
        (output[,2] != output[,3]),
    ]
    if (number_of_responses_value == 4) output <- output[
        (output[,1] != output[,2]) |
        (output[,1] != output[,3]) |
        (output[,1] != output[,4]) |
        (output[,2] != output[,3]) |
        (output[,2] != output[,4]) |
        (output[,3] != output[,4]),
    ]
    
    output <- output[!is.na(rownames(output)),]
    
    return(output)

}

## subfunction 4
checkResponses_mergeRecordIDs <- function(
    data,
    identifier_value,
    survey_type_value, # "baseline_survey", "weekly_survey", "final_survey"
    created_at_date_value,
    modified_at_date_value,
    number_of_responses_value # number of responses per question_id, question_text, and modified_at_date
) {
    
    output <- data
    
    # Merge record-identifying columns back in.
    rownames_output <- rownames(output)
    output <- output %>%
        dplyr::as_tibble(.name_repair = "universal") %>%
        dplyr::mutate(
            
            checkResponses_identifier_variable = identifier_value,
            survey_type = survey_type_value,
            created_at_date = lubridate::as_date(created_at_date_value),
            modified_at_date = lubridate::as_date(modified_at_date_value),
            number_of_responses = number_of_responses_value,
            original_column_name = rownames_output
            
        )
    
    return(output)

}

## main function
checkResponses <- function(
    data,
    identifier_variable,
    identifier_value,
    survey_type_value, # "baseline_survey", "weekly_survey", "final_survey"
    created_at_date_value,
    modified_at_date_value,
    number_of_responses_value # number of responses per question_id, question_text, and modified_at_date
) {
    
    # Filter for identifier_value.
    output <- data
    names(output)[names(output) == identifier_variable] <- "checkResponses_identifier_variable"
    output <- output %>%
        dplyr::filter(checkResponses_identifier_variable == identifier_value) %>%
        dplyr::select(-checkResponses_identifier_variable)
    
    # Run all functions.
    output <- output %>%
        
        checkResponses_filter(
            survey_type_value,
            created_at_date_value,
            modified_at_date_value,
            number_of_responses_value
        ) %>%
        
        checkResponses_pivot() %>%
        
        checkResponses_transpose_filterMismatches(
            number_of_responses_value
        ) %>%
        
        checkResponses_mergeRecordIDs(
            identifier_value,
            survey_type_value,
            created_at_date_value,
            modified_at_date_value,
            number_of_responses_value
        )
    
    # Re-order and rename columns.
    vars_to_select <- c(
        
        "checkResponses_identifier_variable",
        "survey_type",
        "created_at_date",
        "modified_at_date",
        "number_of_responses",
        "original_column_name"
        
    )
    if (number_of_responses_value == 2) output <- output %>%
        dplyr::select(
            
            dplyr::all_of(vars_to_select),
            `...1`,
            `...2`
            
        ) %>%
        dplyr::rename(
            
            answer_text_value_1 = `...1`,
            answer_text_value_2 = `...2`
            
        )
    if (number_of_responses_value == 3) output <- output %>%
        dplyr::select(
            
            dplyr::all_of(vars_to_select),
            `...1`,
            `...2`,
            `...3`
            
        ) %>%
        dplyr::rename(
            
            answer_text_value_1 = `...1`,
            answer_text_value_2 = `...2`,
            answer_text_value_3 = `...3`
            
        )
    if (number_of_responses_value == 4) output <- output %>%
        dplyr::select(
            
            dplyr::all_of(vars_to_select),
            `...1`,
            `...2`,
            `...3`,
            `...4`
            
        ) %>%
        dplyr::rename(
            
            answer_text_value_1 = `...1`,
            answer_text_value_2 = `...2`,
            answer_text_value_3 = `...3`,
            answer_text_value_4 = `...4`
            
        )
    names(output)[names(output) == "checkResponses_identifier_variable"] <- identifier_variable
    
    return(output)
    
}
