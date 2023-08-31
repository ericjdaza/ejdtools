# Functions to add headers to gtsummary output.



## clean up parentheses
add_header_grep_correction <- function(text_string) {
    
    if (grepl("\\(", text_string) == TRUE) text_string <- gsub(
        "\\(",
        "\\\\(",
        text_string
    )
    if (grepl("\\)", text_string) == TRUE) text_string <- gsub(
        "\\)",
        "\\\\)",
        text_string
    )
    if (grepl("\\?", text_string) == TRUE) text_string <- gsub(
        "\\?",
        "\\\\?",
        text_string
    )
    
    return(text_string)
    
}



## add first row
add_header_first_row <- function(
    data, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    data_to_bind, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    header2_label, # = "label_header_..."
    header2_pairs # = NA (if variable is numeric or MECE) or "pairs_...[[1]]" (must also include header2_label)
) {
    
    if (is.na(header2_pairs)) {
        
        # print("add_header_first_row: Entering the first if block")
        output <- data %>%
            dplyr::bind_rows(
                data_to_bind %>%
                    dplyr::filter(
                        1 <= dplyr::row_number(),
                        dplyr::row_number() < min(
                            grep(
                                add_header_grep_correction(header2_label),
                                data_to_bind$variable
                            )
                        )
                    )
            )
        
    }
    
    if (!is.na(header2_pairs) & !is.na(header2_label)) {
        
        # print("add_header_first_row: Entering the second if block")
        output <- data %>%
            dplyr::bind_rows(dplyr::tibble(row_type = "label", label = header2_label)) %>%
            dplyr::bind_rows(
                data_to_bind %>%
                    dplyr::filter(
                        1 <= dplyr::row_number(),
                        dplyr::row_number() < min(
                            grep(
                                add_header_grep_correction(header2_pairs),
                                data_to_bind$variable
                            )
                        )
                    ) %>%
                    dplyr::mutate(row_type = "level")
            )
        
    }
    
    return(output)
    
}



## add middle row
add_header_middle_row <- function(
    data, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    data_to_bind, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    header1_label, # = "label_header_..."
    header1_pairs, # = NA (if variable is numeric or MECE) or "pairs_...[[1]]" (must also include header1_label)
    header2_label, # = "label_header_..."
    header2_pairs # = NA (if variable is numeric or MECE) or "pairs_...[[1]]" (header2_label not needed)
) {
    
    if (is.na(header1_pairs)) {
        
        # print("add_header_middle_row: Entering the first if block")
        # print(paste0("add_header_middle_row: header1_label: ", header1_label))
        # print(paste0("add_header_middle_row: header1_pairs: ", header1_pairs))
        # print(paste0("add_header_middle_row: header2_label: ", header2_label))
        # print(paste0("add_header_middle_row: header2_pairs: ", header2_pairs))
        output <- data %>%
            dplyr::bind_rows(
                data_to_bind %>%
                    dplyr::filter(
                        min(
                            grep(
                                add_header_grep_correction(header1_label),
                                data_to_bind$variable
                            )
                        ) <= dplyr::row_number(),
                        dplyr::row_number() < min(
                            grep(
                                ifelse(
                                    is.na(header2_pairs),
                                    add_header_grep_correction(header2_label),
                                    add_header_grep_correction(header2_pairs)
                                ),
                                data_to_bind$variable
                            )
                        )
                    )
            )
        
    }
    
    if (!is.na(header1_pairs) & !is.na(header1_label)) {
        
        # print("add_header_middle_row: Entering the second if block")
        # print(paste0("add_header_middle_row: header1_label: ", header1_label))
        # print(paste0("add_header_middle_row: header1_pairs: ", header1_pairs))
        # print(paste0("add_header_middle_row: header2_label: ", header2_label))
        # print(paste0("add_header_middle_row: header2_pairs: ", header2_pairs))
        output <- data %>%
            dplyr::bind_rows(dplyr::tibble(row_type = "label", label = header1_label)) %>%
            dplyr::bind_rows(
                data_to_bind %>%
                    dplyr::filter(
                        min(
                            grep(
                                add_header_grep_correction(header1_pairs),
                                data_to_bind$variable
                            )
                        ) <= dplyr::row_number(),
                        dplyr::row_number() < min(
                            grep(
                                ifelse(
                                    is.na(header2_pairs),
                                    add_header_grep_correction(header2_label),
                                    add_header_grep_correction(header2_pairs)
                                ),
                                data_to_bind$variable
                            )
                        )
                    ) %>%
                    dplyr::mutate(row_type = "level")
            )
        
    }
    
    return(output)
    
}



## add last row
add_header_last_row <- function(
    data, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    data_to_bind, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    header1_label, # = "label_header_..."
    header1_pairs # = NA (if variable is numeric or MECE) or "pairs_...[[1]]" (must also include header1_label)
) {
    
    if (is.na(header1_pairs)) {
        
        # print("add_header_last_row: Entering the first if block")
        output <- data %>%
            dplyr::bind_rows(
                data_to_bind %>%
                    dplyr::filter(
                        min(
                            grep(
                                add_header_grep_correction(header1_label),
                                data_to_bind$variable
                            )
                        ) <= dplyr::row_number()
                    )
            )
        
    }
    
    if (!is.na(header1_pairs) & !is.na(header1_label)) {
        
        # print("add_header_last_row: Entering the second if block")
        output <- data %>%
            dplyr::bind_rows(dplyr::tibble(row_type = "label", label = header1_label)) %>%
            dplyr::bind_rows(
                data_to_bind %>%
                    dplyr::filter(
                        min(
                            grep(
                                add_header_grep_correction(header1_pairs),
                                data_to_bind$variable
                            )
                        ) <= dplyr::row_number()
                    ) %>%
                    dplyr::mutate(row_type = "level")
            )
        
    }
    
    return(output)
    
}



## main function
add_header <- function(
    data, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    data_to_bind, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    header1_label = NA, # = NA (i.e., first row) or "label_header_..."
    header1_pairs = NA, # = NA (if variable is numeric or MECE) or "pairs_...[[1]]"
    header2_label = NA, # = "label_header_..." or NA (i.e., last row)
    header2_pairs = NA # = NA (if variable is numeric or MECE) or "pairs_...[[1]]"
) {
    
#     if (
#         is.na(header1_label) &
#         (!is.na(header2_label) | !is.na(header2_pairs))
#     ) {
        
#         # print("add_header: Entering the first if block")
#         # print(paste0("add_header: header1_label: ", header1_label))
#         # print(paste0("add_header: header1_pairs: ", header1_pairs))
#         # print(paste0("add_header: header2_label: ", header2_label))
#         # print(paste0("add_header: header2_pairs: ", header2_pairs))
#         output <- data %>%
#             add_header_first_row(
#                 data_to_bind = data_to_bind,
#                 header2_label = header2_label,
#                 header2_pairs = header2_pairs
#             )
        
#     }
    
#     if (
#         !is.na(header1_label) &
#         (!is.na(header2_label) | !is.na(header2_pairs))
#     ) {
        
#         # print("add_header: Entering the second if block")
#         # print(paste0("add_header: header1_label: ", header1_label))
#         # print(paste0("add_header: header1_pairs: ", header1_pairs))
#         # print(paste0("add_header: header2_label: ", header2_label))
#         # print(paste0("add_header: header2_pairs: ", header2_pairs))
#         output <- data %>%
#             add_header_middle_row(
#                 data_to_bind = data_to_bind,
#                 header1_label = header1_label,
#                 header1_pairs = header1_pairs,
#                 header2_label = header2_label,
#                 header2_pairs = header2_pairs
#             )
        
#     }
    
#     if (
#         !is.na(header1_label) &
#         (is.na(header2_label) & is.na(header2_pairs))
#     ) {
        
#         # print("add_header: Entering the third if block")
#         # print(paste0("add_header: header1_label: ", header1_label))
#         # print(paste0("add_header: header1_pairs: ", header1_pairs))
#         # print(paste0("add_header: header2_label: ", header2_label))
#         # print(paste0("add_header: header2_pairs: ", header2_pairs))
#         output <- data %>%
#             add_header_last_row(
#                 data_to_bind = data_to_bind,
#                 header1_label = header1_label,
#                 header1_pairs = header1_pairs
#             )
        
#     }
    
    # from chatgpt (GPT-3.5)
    if (!is.na(header1_label) &
        (!is.na(header2_label) | !is.na(header2_pairs))) {
        
        # print("add_header: Entering the first elseif block")
        # print(paste0("add_header: header1_label: ", header1_label))
        # print(paste0("add_header: header1_pairs: ", header1_pairs))
        # print(paste0("add_header: header2_label: ", header2_label))
        # print(paste0("add_header: header2_pairs: ", header2_pairs))
        output <- data %>%
            add_header_middle_row(
                data_to_bind = data_to_bind,
                header1_label = header1_label,
                header1_pairs = header1_pairs,
                header2_label = header2_label,
                header2_pairs = header2_pairs
            )
        
    } else if (!is.na(header1_label) &
               (is.na(header2_label) & is.na(header2_pairs))) {
        
        # print("add_header: Entering the second elseif block")
        # print(paste0("add_header: header1_label: ", header1_label))
        # print(paste0("add_header: header1_pairs: ", header1_pairs))
        # print(paste0("add_header: header2_label: ", header2_label))
        # print(paste0("add_header: header2_pairs: ", header2_pairs))
        output <- data %>%
            add_header_last_row(
                data_to_bind = data_to_bind,
                header1_label = header1_label,
                header1_pairs = header1_pairs
            )
        
    } else if (!is.na(header2_label) | !is.na(header2_pairs)) {
        
        # print("add_header: Entering the third elseif block")
        # print(paste0("add_header: header1_label: ", header1_label))
        # print(paste0("add_header: header1_pairs: ", header1_pairs))
        # print(paste0("add_header: header2_label: ", header2_label))
        # print(paste0("add_header: header2_pairs: ", header2_pairs))
        output <- data %>%
            add_header_first_row(
                data_to_bind = data_to_bind,
                header2_label = header2_label,
                header2_pairs = header2_pairs
            )
        
    } else {
        stop("Invalid header configuration")
    }
    
    return(output)
    
}