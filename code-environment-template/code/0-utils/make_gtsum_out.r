# +
# Function for creating gtsummary table with summaries grouped by time interval (i.e., label_header below).
make_gtsum_out <- function(
    data, # input dataset
    label_header = "**Period**", # e.g., "**Period**", "**Week**", "Time Period"
    hide_label = FALSE, # set to TRUE to hide label_header column from output
        multiselect = FALSE, # set to TRUE to properly display variable levels for non-MECE / multiselect responses; only used if hide_label = TRUE
    move_stat_level_values = TRUE, # set to TRUE to move all stat_level values from the label column to the stat_label column of gtsum_out$table_body
    parameter_label = "", # e.g., label_survey_compliance_percent, label_symptomatic
    category_label = NA, # seldom used; also used as topic label
    topic_instead_of_category = FALSE, # seldom used; changes "**Category**" header to "**TOPIC**" and relocates before parameter (instead of after label)
    force_category = FALSE, # set to TRUE for table without "Category" to stack with table with "Category"
    grouping_variable = "device_worn", # e.g., cohort, device, device_worn
    force_continuous = TRUE, # set to TRUE for non-count variables (e.g., percentages)
        type_list = list(
            `Week 1` ~ "continuous2",
            `Week 2` ~ "continuous2",
            `Week 3` ~ "continuous2",
            `Week 4` ~ "continuous2",
            `Week 5` ~ "continuous2",
            `Week 6` ~ "continuous2"
        ), # list of variables to force to continuous; only used if force_continuous = TRUE
    force_label_value = FALSE, # set to TRUE to override column value for label
        label_value = "", # column value to use when displaying overall statistics (i.e., over all
            # grouping_variable values); only used if use_label_value = TRUE
    stat_label_to_use_all_continuous = label_all_continuous # labels to use for all_continuosu variables
){
    
    gtsum_out <- data
    
    if (force_continuous == TRUE) gtsum_out <- gtsum_out %>%

        gtsummary::tbl_summary(

            by = {grouping_variable},
            type = type_list,
            statistic = list(
                all_continuous() ~ statistic_all_continuous,
                all_categorical() ~ statistic_all_categorical
            ),
            digits = list(
                all_continuous() ~ digits_all_continuous,
                all_categorical() ~ digits_all_categorical
            )

        )
    if (force_continuous == FALSE) gtsum_out <- gtsum_out %>%

        gtsummary::tbl_summary(

            by = {grouping_variable},
            statistic = list(
                all_continuous() ~ statistic_all_continuous,
                all_categorical() ~ statistic_all_categorical
            ),
            digits = list(
                all_continuous() ~ digits_all_continuous,
                all_categorical() ~ digits_all_categorical
            )

        )
    
    gtsum_out <- gtsum_out %>%

        # update the column header; see gtsum_out$table_styling
        gtsummary::modify_header(all_stat_cols() ~ "**{level} (N={n})**") %>%

        # Add column with overall summary statistics (https://www.danieldsjoberg.com/gtsummary/reference/add_overall.html)
        gtsummary::add_overall(col_label = "**Overall (N={N})**") %>%

        # add a new column with statistic labels
        gtsummary::add_stat_label(

            location = "column",
            label = list(
                all_continuous() ~ stat_label_to_use_all_continuous,
                all_categorical() ~ label_all_categorical
            )

        )


    # add left-most column with parameter_label for proper table concatenation by tbl_stack(); see gtsum_out$table_body
    # if applicable, add "Category" column for proper table concatenation by tbl_stack(); see gtsum_out$table_body
    if (topic_instead_of_category == FALSE) label_category <- "**Category**"
    if (topic_instead_of_category == TRUE) label_category <- "**Topic**"
    if (is.na(category_label)) {
        
        if (force_category == FALSE) gtsum_out <- gtsum_out %>%

            gtsummary::modify_table_body(

                ~ .x %>%
                dplyr::mutate(parameter = ifelse(dplyr::row_number() == 1, parameter_label, "")) %>%
                dplyr::relocate(parameter, .before = variable) %>%

                # also move "Overall" column all the way to the right
                dplyr::relocate(stat_0, .after = stat_3) %>%

                # also move "Apple Watch" column before "Overall" column
                dplyr::relocate(stat_1, .before = stat_0)

            ) %>%

            # update the column header after modify_table_body(); see gtsum_out$table_styling
            gtsummary::modify_header(

                parameter = label_variable_header,
                label = label_header

            ) %>%
    
            # update the column alignment; see gtsum_out$table_styling
            gtsummary::modify_column_alignment(

                columns = c(parameter, label, stat_label, stat_1, stat_2, stat_3, stat_0),
                align = "left"

            )
        
        if (force_category == TRUE) {
            
            if (topic_instead_of_category == FALSE) gtsum_out <- gtsum_out %>%
            
                gtsummary::modify_table_body(

                    ~ .x %>%
                    dplyr::mutate(
                        parameter = ifelse(dplyr::row_number() == 1, parameter_label, ""),
                        category = ""
                    ) %>%
                    dplyr::relocate(parameter, .before = variable) %>%
                    dplyr::relocate(category, .after = label) %>%

                    # also move "Overall" column all the way to the right
                    dplyr::relocate(stat_0, .after = stat_3) %>%

                    # also move "Apple Watch" column before "Overall" column
                    dplyr::relocate(stat_1, .before = stat_0)

                )
            
            if (topic_instead_of_category == TRUE) gtsum_out <- gtsum_out %>%
            
                gtsummary::modify_table_body(

                    ~ .x %>%
                    dplyr::mutate(
                        parameter = ifelse(dplyr::row_number() == 1, parameter_label, ""),
                        category = ""
                    ) %>%
                    dplyr::relocate(parameter, .before = variable) %>%
                    dplyr::relocate(category, .before = parameter) %>%

                    # also move "Overall" column all the way to the right
                    dplyr::relocate(stat_0, .after = stat_3) %>%

                    # also move "Apple Watch" column before "Overall" column
                    dplyr::relocate(stat_1, .before = stat_0)

                )
            
        }
        
        gtsum_out <- gtsum_out %>%

            # update the column header after modify_table_body(); see gtsum_out$table_styling
            gtsummary::modify_header(

                parameter = label_variable_header,
                label = label_header,
                category = label_category

            ) %>%

            # update the column alignment; see gtsum_out$table_styling
            gtsummary::modify_column_alignment(

                columns = c(parameter, label, category, stat_label, stat_1, stat_2, stat_3, stat_0),
                align = "left"

            )
        
    }
    
    if (!is.na(category_label))
    
        if (topic_instead_of_category == FALSE)  gtsum_out <- gtsum_out %>%

            gtsummary::modify_table_body(

                ~ .x %>%
                dplyr::mutate(
                    parameter = ifelse(dplyr::row_number() == 1, parameter_label, ""),
                    category = ifelse(dplyr::row_number() == 1, category_label, "")
                ) %>%
                dplyr::relocate(parameter, .before = variable) %>%
                dplyr::relocate(category, .after = label) %>%
    #             dplyr::relocate(category, .after = parameter) %>%

                # also move "Overall" column all the way to the right
                dplyr::relocate(stat_0, .after = stat_3) %>%

                # also move "Apple Watch" column before "Overall" column
                dplyr::relocate(stat_1, .before = stat_0)

            )
    
        if (topic_instead_of_category == TRUE)  gtsum_out <- gtsum_out %>%

            gtsummary::modify_table_body(

                ~ .x %>%
                dplyr::mutate(
                    parameter = ifelse(dplyr::row_number() == 1, parameter_label, ""),
                    category = ifelse(dplyr::row_number() == 1, category_label, "")
                ) %>%
                dplyr::relocate(parameter, .before = variable) %>%
                dplyr::relocate(category, .before = parameter) %>%
    #             dplyr::relocate(category, .after = parameter) %>%

                # also move "Overall" column all the way to the right
                dplyr::relocate(stat_0, .after = stat_3) %>%

                # also move "Apple Watch" column before "Overall" column
                dplyr::relocate(stat_1, .before = stat_0)

            )
    
        gtsum_out <- gtsum_out %>%

            # update the column header after modify_table_body(); see gtsum_out$table_styling
            gtsummary::modify_header(

                parameter = label_variable_header,
                label = label_header,
                category = label_category

            ) %>%

            # update the column alignment; see gtsum_out$table_styling
            gtsummary::modify_column_alignment(

                columns = c(parameter, label, category, stat_label, stat_1, stat_2, stat_3, stat_0),
                align = "left"

            )
    
    # for proper display of "overall" summaries across all grouping_variable levels:
    # add label column value or leave empty
    if (force_label_value == TRUE) gtsum_out <- gtsum_out %>%
        gtsummary::modify_table_body(

            ~ .x %>%
            dplyr::mutate(label = label_value)

        )
    
    gtsum_out <- gtsum_out %>%

        # Bold the label and level values.
        gtsummary::bold_labels() %>%
        gtsummary::bold_levels() %>%

        # Add the caption.
        gtsummary::modify_caption(label_caption)
     
    # Move all stat_level values from the label column to the stat_label column of gtsum_out$table_body for proper display.
    if (move_stat_level_values == TRUE) gtsum_out$table_body <- gtsum_out$table_body %>%
        dplyr::mutate(
            
            stat_label = ifelse(row_type == "level", label, stat_label),
            label = ifelse(row_type == "level", "", label)
            
        )
    
    # Hide label_header column from output if desired. Move all labels under "parameter" column.
    if (hide_label == TRUE) {
        
        gtsum_out$table_styling$header <- gtsum_out$table_styling$header %>%
            dplyr::mutate(hide = ifelse(column == "label", TRUE, hide))
        if (multiselect == FALSE) gtsum_out$table_body <- gtsum_out$table_body %>%
            dplyr::mutate(parameter = ifelse(row_type == "level", label, parameter))
        if (multiselect == TRUE) {
            
            gtsum_out$table_body <- gtsum_out$table_body %>%
                dplyr::mutate(
                    parameter = label,
                    row_type = "level"
                )
            
            # add header row displaying parameter_label
            gtsum_out$table_body <- gtsum_out$table_body[0,] %>%
                dplyr::bind_rows(
                    dplyr::tibble(
                        parameter = parameter_label,
                        row_type = "label"
                    )
                ) %>%
                dplyr::bind_rows(gtsum_out$table_body)
            
        }
        
    }
   
    return(gtsum_out)

}
