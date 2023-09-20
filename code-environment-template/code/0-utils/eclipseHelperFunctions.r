# Eclipse study helper functions.



# adapted from https://stackoverflow.com/questions/70767375/is-it-possible-to-transpose-tbl-summary-table-pivot-table
# 
# This function does not assume all possible responses are observed; i.e., there need not be at least one row for each
# response level for each variable listed in vars_to_tabulate. If this does hold, to save a little time you can instead
# set fix_wrong_headers = FALSE.
# 
# Setting fix_wrong_headers = TRUE fixes the following error message from gtsummary::tbl_stack() produced by the main code:
# 
#   ℹ Column headers among stacked tables differ. Headers from the first table are
#   used. Use `quiet = TRUE` to suppress this message.
# 
# This function uses that code to set up the tbl_summary() objects, after which fix_wrong_headers = TRUE fixes this error.
# Hence, gtsummary::tbl_stack(quiet = TRUE) is set below to suppress this error message.


## main function
gtsummaryWrapperEclipseSO6Helper <- function(
    data,
    vars_to_tabulate,
    statistic_all_continuous = "{median} ({p25}, {p75})", # tbl_summary() default
    statistic_all_categorical = "{n} ({p}%)", # tbl_summary() default
    digits_all_continuous = NULL, # use to specify digits in tbl_summary(); must be specified together with digits_all_categorical
    digits_all_categorical = NULL, # use to specify digits in tbl_summary(); must be specified together with digits_all_continuous
    modify_header_label = "Characteristic", # modify_header(label) default
    modify_header_all_stat_cols = "**{level} (N={n})**", # modify_header(all_stat_cols()) argument; default = "**{level}**"
    label_all_continuous = NULL, # use to specify digits in add_stat_label(); must be specified together with label_all_categorical
    label_all_categorical = NULL, # use to specify digits in add_stat_label(); must be specified together with label_all_continuous
    relocate_c = NULL, # use to re-order column names of table_body object of tbl_summary() output
    label_caption = NULL, # modify_caption() argument
    debugging = FALSE,
    fix_wrong_headers = TRUE, # set to FALSE for "favorable" questions
    pairs_responses = NULL, # only used if fix_wrong_headers = TRUE
    stat_num_align = "center" # only used if fix_wrong_headers = TRUE; use this to match the alignment requested in the original
        # gtsummaryWrapper(align) call to specify modify_column_alignment(); allowed values: "left", "right", "center"
) {
    
    # loop over every column in df to construct table
    output <- vars_to_tabulate %>%
    
            map(
                
                # build summary table for single category
                ~ data %>%
                
                    dplyr::select(dplyr::all_of(.x)) %>%
                
                    dplyr::mutate(..true.. = TRUE) %>%
                
                    dplyr::filter(complete.cases(.)) %>%
                
                    gtsummaryWrapper(
                        group = dplyr::all_of(.x),
                        tbl_summary_label = list(..true.. = .x),
                        statistic_all_continuous = statistic_all_continuous,
                        statistic_all_categorical = statistic_all_categorical,
                        digits_all_continuous = digits_all_continuous,
                        digits_all_categorical = digits_all_categorical,
                        modify_header_label = modify_header_label,
                        modify_header_all_stat_cols = modify_header_all_stat_cols,
                        label_all_continuous = label_all_continuous,
                        label_all_categorical = label_all_categorical,
                        label_caption = label_caption,
                        debugging = debugging
                    )
                
            ) %>%
        
        # stack all tbls together
        gtsummary::tbl_stack(quiet = TRUE)
        
    # Re-order columns; see output$table_body.
    if (!is.null(relocate_c)) {

        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: Starting if block 1")
        output <- output %>%
            gtsummary::modify_table_body(

                ~ .x %>%
                dplyr::relocate({relocate_c})

            )
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: Ending if block 1")

    }

    output <- output %>%

        # remove all footnotes
        gtsummary::modify_footnote(all_stat_cols() ~ NA) %>%

        # modify label and stat_label display
        gtsummary::modify_column_merge(pattern = "{label}")
    
    if (fix_wrong_headers == TRUE) {
        
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: Starting if block 2")
        
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: If block 2: Starting chunk 1")
        output$table_body <- data %>%
            dplyr::select(identifier, {vars_to_tabulate}) %>%
            tidyr::pivot_longer(
                cols = !identifier,
                names_to = "label",
                values_to = "response"
            ) %>%
            dplyr::group_by(

                label,
                response

            ) %>%
            dplyr::tally() %>%
            dplyr::ungroup() %>%
            dplyr::group_by(label) %>%
            tidyr::pivot_wider(

                id_cols = label,
                names_from = response,
                values_from = n

            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(-`NA`)
        output$table_body[is.na(output$table_body)] <- 0
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: If block 2: Ending chunk 1")

        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: If block 2: Starting chunk 2")
        output$table_body <- output$table_body %>%
            dplyr::mutate_at(
                vars(as.character(pairs_responses)),
                as.character
            ) %>%
            dplyr::mutate(

                tbl_id1 = seq(1, nrow(output$table_body), 1) %>% as.integer(),
                variable = "..true..",
                var_type = "dichotomous",
                var_label = label,
                row_type = "label",
                stat_label = label_all_categorical

            )
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: If block 2: Ending chunk 2")

        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: If block 2: Starting chunk 3")
        for (header_label in as.character(pairs_responses)) names(output$table_body)[names(output$table_body) == header_label] <- names(pairs_responses)[pairs_responses == header_label]
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: If block 2: Ending chunk 3")
        
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: If block 2: Starting chunk 4")
        output$table_styling$header <- output$table_styling$header %>%
            dplyr::filter(!(column %in% names(pairs_responses))) %>%
            dplyr::bind_rows(
                y = dplyr::tibble(

                    column = names(pairs_responses),
                    hide = FALSE,
                    align = stat_num_align,
                    interpret_label = "gt::md",
                    label = paste0("**", as.character(pairs_responses), "**"),
                    interpret_spanning_header = "gt::md",
                    spanning_header = NA,
                    modify_stat_N = output$table_styling$header$modify_stat_N[output$table_styling$header$column == "variable"],
                    modify_stat_n = NA,
                    modify_stat_p = NA,
                    modify_stat_level = as.character(pairs_responses)

                )
            )
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: If block 2: Ending chunk 4")
        
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper: Ending if block 2")
        
    }
    
    output
    
}


## subfunction
gtsummaryWrapperEclipseSO6Helper2 <- function(
    data,
    data_favorable,
    label_header_favorable,
    satisfaction = TRUE,
    relocate_c = NULL,
    pairs,
    debugging = FALSE
) {
    
    # Get stat_ columns.
    all_stat_columns <- data$table_styling$header$column[
        grepl("stat_", data$table_styling$header$column) &
        !grepl("stat_label", data$table_styling$header$column)
    ]
    if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper2: all_stat_columns:")
    if (debugging == TRUE) print(all_stat_columns)
    
    # Add favorable column.
    data$table_body <- data$table_body %>%
        dplyr::full_join(
            y = data_favorable$table_body %>%
                dplyr::mutate(label = gsub("favorable_", "", label)) %>%
                dplyr::select(

                    label,
                    stat_2

                ) %>%
                dplyr::rename(stat_favorable = stat_2),
            by = "label"
        )
    data$table_styling$header <- data$table_styling$header %>%
        dplyr::bind_rows(
            y = data_favorable$table_styling$header %>%
                dplyr::filter(column == "stat_2") %>%
                dplyr::mutate(

                    column = "stat_favorable",
                    label = ifelse(
                        label == "**TRUE**",
                        paste0("**", label_header_favorable, "**"),
                        label
                    )
                )
        )
    
    # Add number of responses column.
    if (satisfaction == TRUE) {
        
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper2: Starting if block 1")
        data$table_body <- data$table_body %>%
            # dplyr::mutate(stat_number_of_responses = data$table_styling$header$modify_stat_N[data$table_styling$header$column == "stat_1"])
            dplyr::mutate(

                num_stat_1 = stat_1 %>% as.numeric(),
                num_stat_2 = stat_2 %>% as.numeric(),
                num_stat_3 = stat_3 %>% as.numeric(),
                num_stat_4 = stat_4 %>% as.numeric(),
                num_stat_5 = stat_5 %>% as.numeric(),

                stat_number_of_responses = rowSums(
                    dplyr::pick(

                        num_stat_1,
                        num_stat_2,
                        num_stat_3,
                        num_stat_4,
                        num_stat_5

                    ),
                    na.rm = TRUE
                )

            )
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper2: Ending if block 1")
        
    }
    if (satisfaction == FALSE) {
        
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper2: Starting if block 2")
        data$table_body <- data$table_body %>%
            # dplyr::mutate(stat_number_of_responses = data$table_styling$header$modify_stat_N[data$table_styling$header$column == "stat_1"])
            dplyr::mutate(

                num_stat_1 = stat_1 %>% as.numeric(),
                num_stat_2 = stat_2 %>% as.numeric(),
                num_stat_3 = stat_3 %>% as.numeric(),
                num_stat_4 = stat_4 %>% as.numeric(),
                num_stat_5 = stat_5 %>% as.numeric(),
                num_stat_6 = stat_6 %>% as.numeric(),
                num_stat_7 = stat_7 %>% as.numeric(),
                num_stat_8 = stat_8 %>% as.numeric(),
                num_stat_9 = stat_9 %>% as.numeric(),
                num_stat_10 = stat_10 %>% as.numeric(),
                num_stat_11 = stat_11 %>% as.numeric(),

                stat_number_of_responses = rowSums(
                    dplyr::pick(

                        num_stat_1,
                        num_stat_2,
                        num_stat_3,
                        num_stat_4,
                        num_stat_5,
                        num_stat_6,
                        num_stat_7,
                        num_stat_8,
                        num_stat_9,
                        num_stat_10,
                        num_stat_11

                    ),
                    na.rm = TRUE
                )

            )
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper2: Ending if block 2")
        
    }
    data$table_body <- data$table_body %>%
        dplyr::select(-dplyr::starts_with("num_stat_"))
    data$table_styling$header <- data$table_styling$header %>%
        dplyr::bind_rows(
            y = data$table_styling$header %>%
                dplyr::filter(column == "stat_1") %>% # use as template
                dplyr::mutate(
    
                    column = "stat_number_of_responses",
                    label = paste0("**", label_header_number_of_responses, "**"),
                    modify_stat_n = modify_stat_N,
                    modify_stat_p = 1,
                    modify_stat_level = label_header_number_of_responses
                    
                )
        )
    
    # Re-order columns; see gtsum_out$table_body.
    if (!is.null(relocate_c)) {

        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper2: Starting if block 3")
        data <- data %>%
            gtsummary::modify_table_body(

                ~ .x %>%
                dplyr::relocate({relocate_c})

            )
        if (debugging == TRUE) print("gtsummaryWrapperEclipseSO6Helper2: Ending if block 3")

    }
    
    # Add percentages.
    for (stat_column in all_stat_columns)
        for (j in 1:nrow(data$table_body)) data$table_body[[j, stat_column]] <- ifelse(
            data$table_body[[j, stat_column]] == 0,
            "0",
            paste0(
                data$table_body[[j, stat_column]],
                " (",
                round(as.numeric(data$table_body[[j, stat_column]]) / as.numeric(data$table_body[[j, "stat_number_of_responses"]]) * 100, 1),
                "%)"
            )
        )
    data$table_body <- data$table_body %>%
        dplyr::mutate(
            stat_favorable = ifelse(
                stat_favorable == 0,
                "0",
                paste0(stat_favorable, " (", round(as.numeric(stat_favorable) / as.numeric(stat_number_of_responses) * 100, 1), "%)")
            )
        )
    
    # Re-label variables.
    data$table_body$label <- pairs[data$table_body$label] %>% as.character()
    
    data
    
}



# Create all possible study days and weeks for all provided study identifiers.
# Label study days for needed study periods.
completeAllDaysAndWeeks <- function(
    data,
    identifier, # must be in dataset
    study_start_datevar, # must be in dataset; study day 1 is set as as.Date(study_start_datevar)
    datevar, # must be in dataset
    total_study_days = 90,
    first_last_7_days = FALSE, # TRUE for SAP Section 7.3.5.1 Secondary Endpoint 5.1 (SE5.1)
    first_last_14_days = TRUE, # TRUE for SAP Section 7.3.3 Secondary Objective 3 (SO3)
    first_last_30_days = FALSE, # TRUE for SAP Section 7.3.5.1 Secondary Endpoint 5.1 (SE5.1)
    debugging = FALSE
) {
    
    # function_args <- c(as.list(environment())) # source: https://stackoverflow.com/questions/11885207/get-all-parameters-as-list
    
    names(data)[names(data) == identifier] <- "makeAllDaysAndWeeks_identifier"
    names(data)[names(data) == study_start_datevar] <- "makeAllDaysAndWeeks_study_start_datevar"
    names(data)[names(data) == datevar] <- "makeAllDaysAndWeeks_datevar"
    
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Starting data1 <- data")
    data1 <- data %>%
        mutate(
            
            makeAllDaysAndWeeks_study_start_datevar_as_Date = makeAllDaysAndWeeks_study_start_datevar %>% as.Date(),
            makeAllDaysAndWeeks_datevar_as_Date = makeAllDaysAndWeeks_datevar %>% as.Date(),
            actual_response = TRUE
            
        )
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Ending data1 <- data")
    
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Starting data2 <- data1")
    data2 <- data1 %>%
        dplyr::distinct(

            makeAllDaysAndWeeks_identifier,
            makeAllDaysAndWeeks_study_start_datevar_as_Date,
            makeAllDaysAndWeeks_datevar_as_Date,
            actual_response

        ) %>%
        dplyr::group_by(makeAllDaysAndWeeks_identifier) %>%
        dplyr::mutate(
            
            min_makeAllDaysAndWeeks_datevar_as_Date = min(makeAllDaysAndWeeks_datevar_as_Date, na.rm = TRUE),
            max_makeAllDaysAndWeeks_datevar_as_Date = max(makeAllDaysAndWeeks_datevar_as_Date, na.rm = TRUE)
            
        ) %>%
        tidyr::complete(
            makeAllDaysAndWeeks_datevar_as_Date = seq.Date(
                min(
                    min_makeAllDaysAndWeeks_datevar_as_Date,
                    makeAllDaysAndWeeks_study_start_datevar_as_Date
                ),
                max(
                    max_makeAllDaysAndWeeks_datevar_as_Date,
                    makeAllDaysAndWeeks_study_start_datevar_as_Date + lubridate::days(total_study_days - 1)
                ),
                by = "day"
            ),
            fill = list(actual_response = FALSE)
        ) %>%
        dplyr::ungroup()
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Ending data2 <- data1")
    
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Starting min_max_datevar <- data2")
    min_max_datevar <- data2 %>%
        dplyr::distinct(
            
            makeAllDaysAndWeeks_identifier,
            makeAllDaysAndWeeks_study_start_datevar_as_Date,
            min_makeAllDaysAndWeeks_datevar_as_Date,
            max_makeAllDaysAndWeeks_datevar_as_Date
            
        )
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Ending min_max_datevar <- data2")
    
    names(min_max_datevar)[names(min_max_datevar) == "makeAllDaysAndWeeks_identifier"] <- identifier
    
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Starting data3 <- data2")
    data3 <- data2 %>%
        dplyr::select(-c(
            
            makeAllDaysAndWeeks_study_start_datevar_as_Date,
            min_makeAllDaysAndWeeks_datevar_as_Date,
            max_makeAllDaysAndWeeks_datevar_as_Date
        
        )) %>%
        dplyr::left_join(
            y = data1 %>% # re-merge to make sure makeAllDaysAndWeeks_study_start_datevar_as_Date is on every row for calculations
                dplyr::distinct(
                    
                    makeAllDaysAndWeeks_identifier,
                    makeAllDaysAndWeeks_study_start_datevar_as_Date
                    
                ),
            by = "makeAllDaysAndWeeks_identifier"
        ) %>%
        dplyr::mutate(
            
            participant_study_day = as.numeric(makeAllDaysAndWeeks_datevar_as_Date - makeAllDaysAndWeeks_study_start_datevar_as_Date + 1),
            participant_study_week = ceiling(participant_study_day/7),
            
            in_study_period_raw = (
                0 < participant_study_day &
                participant_study_day <= total_study_days
            )
                        
        ) %>%
        dplyr::select(-makeAllDaysAndWeeks_study_start_datevar_as_Date)
    if (first_last_7_days == TRUE) data3 <- data3 %>%
        dplyr::mutate(
            
            first_7_days = (
                0 < participant_study_day &
                participant_study_day <= 7
            ),
            last_7_days = (
                total_study_days - 7 < participant_study_day &
                participant_study_day <= total_study_days
            ),
            
            first_last_week_label_raw = dplyr::case_when(
                first_7_days == FALSE & last_7_days == FALSE ~ "neither",
                first_7_days == TRUE & last_7_days == FALSE ~ "first",
                first_7_days == FALSE & last_7_days == TRUE ~ "last",
                first_7_days == TRUE & last_7_days == TRUE ~ "both"
            )
            
        )
    if (first_last_14_days == TRUE) data3 <- data3 %>%
        dplyr::mutate(
            
            first_14_days = (
                0 < participant_study_day &
                participant_study_day <= 14
            ),
            last_14_days = (
                total_study_days - 14 < participant_study_day &
                participant_study_day <= total_study_days
            ),
            
            fortnight_label_raw = dplyr::case_when(
                first_14_days == FALSE & last_14_days == FALSE ~ "neither",
                first_14_days == TRUE & last_14_days == FALSE ~ "first",
                first_14_days == FALSE & last_14_days == TRUE ~ "last",
                first_14_days == TRUE & last_14_days == TRUE ~ "both"
            )
            
        )
    if (first_last_30_days == TRUE) data3 <- data3 %>%
        dplyr::mutate(
            
            first_30_days = (
                0 < participant_study_day &
                participant_study_day <= 30
            ),
            last_30_days = (
                total_study_days - 30 < participant_study_day &
                participant_study_day <= total_study_days
            ),
            
            first_last_30_days_label_raw = dplyr::case_when(
                first_30_days == FALSE & last_30_days == FALSE ~ "neither",
                first_30_days == TRUE & last_30_days == FALSE ~ "first",
                first_30_days == FALSE & last_30_days == TRUE ~ "last",
                first_30_days == TRUE & last_30_days == TRUE ~ "both"
            )
            
        )
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Ending data3 <- data2")
    
    names(data3)[names(data3) == "makeAllDaysAndWeeks_identifier"] <- identifier
    
    names(data1)[names(data1) == "makeAllDaysAndWeeks_identifier"] <- identifier
    names(data1)[names(data1) == "makeAllDaysAndWeeks_study_start_datevar"] <- study_start_datevar
    names(data1)[names(data1) == "makeAllDaysAndWeeks_datevar"] <- datevar
    data1$actual_response <- NULL
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Starting data4 <- data3")
    data4 <- data3 %>%
        dplyr::distinct() %>%
        dplyr::full_join(
            y = data1 %>% distinct(),
            by = c(
                identifier,
                "makeAllDaysAndWeeks_datevar_as_Date"
            ),
            multiple = "all"
        )
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Ending data4 <- data3")
    
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Starting list_output <- list(")
    list_output <- list(
        
        output = data3,
        output_with_input = data4,
        min_max_datevar = min_max_datevar
        
    )
    if (debugging == TRUE) print("completeAllDaysAndWeeks: Ending list_output <- list(")
    
    list_output
    
}



# not used; save just in case
getLastWeekOfEclipsePartnerData <- function(data) {
    
    last_weeks <- data %>%
        dplyr::filter(
            !is.na(participant_id),
            !is.na(week_of)
        ) %>%
        dplyr::distinct(
            participant_id,
            week_of
        ) %>%
        dplyr::arrange(
            participant_id,
            desc(week_of)
        ) %>%
        dplyr::group_by(participant_id) %>%
        dplyr::filter(dplyr::row_number() == 1) %>%
        dplyr::ungroup()
    
    data <- data %>%
        dplyr::right_join(
            y = last_weeks,
            by = c(

                "participant_id",
                "week_of"

            )
        )
    
    data
    
}



# OTR data processing functions.

## Take the sum of outcome_to_sum for a given sum_interval.
OTRSumOutcomesCoreFunction <- function(
    data, # OTR dataset
    source, # string variable that generally corresponds to data argument
    outcome_to_sum, # OTR variable to sum
    datevar = "start_date", # OTR date variable; must be identical between data and data2
    sum_interval = "calendar_month" # "calendar_month" or "week" (i.e., week starting at and including datevar)
) {
    
    names(data)[names(data) == datevar] <- "OTRSumOutcomesCoreFunction_datevar"
    names(data)[names(data) == outcome_to_sum] <- "OTRSumOutcomesCoreFunction_outcome_to_sum"
    data <- data %>%
        dplyr::distinct(

            participant_id,
            OTRSumOutcomesCoreFunction_datevar,
            OTRSumOutcomesCoreFunction_outcome_to_sum

        )
    
    if (sum_interval == "calendar_month") data <- data %>%
        dplyr::mutate(sum_interval = format(as.Date(OTRSumOutcomesCoreFunction_datevar), "%Y-%m")) %>%
        dplyr::select(-OTRSumOutcomesCoreFunction_datevar)
    if (sum_interval == "week") data <- data %>%
        dplyr::rename(sum_interval = OTRSumOutcomesCoreFunction_datevar)
    
    data <- data %>%
        dplyr::distinct() %>%
        dplyr::group_by(

            participant_id,
            sum_interval

        ) %>%
        dplyr::summarize(summed_outcome = sum(OTRSumOutcomesCoreFunction_outcome_to_sum)) %>%
        dplyr::ungroup()
    
    data <- data %>%
        dplyr::arrange(

            participant_id,
            sum_interval

        )
    names(data)[names(data) == "sum_interval"] <- sum_interval
    
    data <- data %>%
        dplyr::mutate(sources = paste0(source, "__", outcome_to_sum))
    
    data
    
}

## Main function.
OTRSumOutcomes <- function(
    data, # OTR dataset
    source, # string variable that generally corresponds to data argument
    data2 = NULL, # OTR dataset (optional)
    source2 = NULL, # string variable that generally corresponds to data argument (optional)
    outcome_to_sum, # OTR variable to sum; must be identical between data and data2
    datevar = "start_date", # OTR date variable; must be identical between data and data2
    sum_interval = "calendar_month" # "calendar_month" or "week" (i.e., week starting at and including datevar)
) {
    
    data <- data %>%
        OTRSumOutcomesCoreFunction(
            source = source,
            datevar = datevar,
            outcome_to_sum = outcome_to_sum,
            sum_interval = sum_interval
        )
    
    if (!is.null(data2)) data <- data %>%
        dplyr::full_join(
            y = data2 %>%
                OTRSumOutcomesCoreFunction(
                    source = source2,
                    datevar = datevar,
                    outcome_to_sum = outcome_to_sum,
                    sum_interval = sum_interval
                ),
            by = c(

                "participant_id",
                sum_interval

            )
        ) %>%
        dplyr::mutate(sources = paste(sources.x, ", ", sources.y) %>% removeNAFromString()) %>%
        dplyr::rowwise() %>%
        dplyr::mutate_at(
            vars(dplyr::matches("summed_outcome.")),
            function(x) ifelse(is.na(x), 0, x)
        ) %>%
        dplyr::mutate(summed_outcome = rowSums(dplyr::pick(dplyr::matches("summed_outcome.")))) %>%
        dplyr::select(-c(
            dplyr::matches("summed_outcome."),
            dplyr::matches("sources.")
        ))
    
    names(data)[names(data) == sum_interval] <- "sum_interval"
    data <- data %>%
        dplyr::arrange(

            participant_id,
            sum_interval

        )
    names(data)[names(data) == "sum_interval"] <- sum_interval
    
    data
    
}

## Take the mean and median of outcome_for_mean_and_median for each participant.
## Do this over all consecutive sum_intervals (i.e., from the first observed
## sum_interval to the last).
OTRMeanMedianOutcomes <- function(
    data, # OTR dataset
    complete_sum_intervals = TRUE, # TRUE to aggregate over all consecutive sum_intervals (i.e., from the first observed
        # sum_interval to the last); FALSE to only aggregate over sum_intervals with non-zero outcome_for_mean_and_median
        # values
    outcome_for_mean_and_median = "summed_outcome", # OTR-based variable for which to take the mean and median per participant;
        # "summed_outcome" default from OTRSumOutcomes() objects
    sum_interval = "calendar_month" # "calendar_month" or "week" (i.e., week starting at and including OTRSumOutcomes(datevar));
        # "calendar_month" default from OTRSumOutcomes() objects
) {
    
    function_args <- c(as.list(environment())) # source: https://stackoverflow.com/questions/11885207/get-all-parameters-as-list
    
    mau_vars_to_check <- names(data)[names(data) != "participant_id"]
    
    names(data)[names(data) == outcome_for_mean_and_median] <- "OTRMeanMedianOutcomes_outcome_for_mean_and_median"
    names(data)[names(data) == sum_interval] <- "OTRMeanMedianOutcomes_sum_interval"
    
    if (complete_sum_intervals == TRUE) {

        if (sum_interval == "calendar_month") complete_by <- "month"
        if (sum_interval == "week") complete_by <- "week"
        
        # for calendar_month, change date format to month on day 1 for proper processing
        if (sum_interval == "calendar_month") data <- data %>%
            dplyr::mutate(OTRMeanMedianOutcomes_sum_interval = lubridate::ymd(paste0(OTRMeanMedianOutcomes_sum_interval, "-01")))

        data <- data %>%
            dplyr::group_by(participant_id) %>%
            tidyr::complete(
                OTRMeanMedianOutcomes_sum_interval = seq.Date(
                    min(OTRMeanMedianOutcomes_sum_interval),
                    max(OTRMeanMedianOutcomes_sum_interval),
                    by = complete_by
                ),
                fill = list(OTRMeanMedianOutcomes_outcome_for_mean_and_median = 0)
            )
        
    }
    
    if (complete_sum_intervals == FALSE) data <- data[0 < data$OTRMeanMedianOutcomes_outcome_for_mean_and_median,]
    
    data <- data %>%
        dplyr::group_by(participant_id) %>%
        dplyr::summarize(
            
            mean_outcome = mean(OTRMeanMedianOutcomes_outcome_for_mean_and_median),
            median_outcome = median(OTRMeanMedianOutcomes_outcome_for_mean_and_median),
            n_obs_sum_interval = dplyr::n(), # number of observations used
            sources = paste0(unique(sources[!is.na(sources)]), collapse = ", ") %>% removeNAFromString()
        
        ) %>%
        dplyr::ungroup()
    
    output_list <- list(
        mau_data = function_args$data,
        mau_vars_to_check = mau_vars_to_check,
        output_dataset = data
    )
    
    output_list
    
}



# Monthly Active Users (MAU) functions.

## Identify individuals and dates they are active for a given dataset.
identifyMAUrows <- function(
    data,
    source, # string variable that generally corresponds to data argument; not used if mode = "mau"
    datevar, # date variable to use; can be "calendar_month"; not used if mode = "filter"
    datevar_type = "date", # "date" or "calendar_month"; not used if mode = "filter"
    vars_to_check = NULL, # require at least one of these variables to not be missing/NA to not be filtered out
    vars_gt_0 = NULL, # require these variables to be >0 to not be filtered out;
        # set to "summed_outcome" if data is an OTRMeanMedianOutcomes()$mau_data object
    vars_not_NA = NULL, # require these variables to be not missing/NA to not be filtered out
    mode = "both", # "mau" to filter and then keep only idvar, datevar, mau_source, and mau_datevar;
        # "filter" to simply filter rows for subsequent analysis
        # "both" to "filter" then "mau"
    idvar = "participant_id"
) {
    
    # function_args$data[0 < function_args$data[outcome_for_mean_and_median],], # only keep rows with outcome_for_mean_and_median > 0
    
    # filter: only keep rows that indicate ≥1 activity was observed defined as
    # having at least one of all vars_to_check not missing/NA
    if (mode %in% c("both","filter")) {
        
        if (!is.null(vars_to_check)) {
            
            data$sum_is_na <- data %>%
                dplyr::select({vars_to_check}) %>%
                apply(
                    1,
                    function(x) sum(is.na(x))
                )
            # data$sum_is_na <- apply(
            #     data[vars_to_check],
            #     1,
            #     function(x) sum(is.na(x))
            # )
            data <- data %>%
                dplyr::filter(sum_is_na < length(vars_to_check))
            data$sum_is_na <- NULL
            
        }
        if (!is.null(vars_gt_0)) {for (vars_gt_0_element in vars_gt_0) data <- data[data[vars_gt_0_element] > 0,]}
        if (!is.null(vars_not_NA)) {for (vars_not_NA_element in vars_not_NA) data <- data[!is.na(data[vars_not_NA_element]),]}
        
    }
    
    # mau: identify active idvar values per month
    if (mode %in% c("both","mau")) {
        
        names(data)[names(data) == idvar] <- "idvar"
        datevar_string <- datevar
        names(data)[names(data) == datevar] <- "datevar"
        data <- data %>%
            dplyr::distinct(

                idvar,
                datevar

            ) %>%
            dplyr::arrange(

                idvar,
                datevar

            ) %>%
            dplyr::mutate(

                mau_source = source,
                mau_datevar = paste0(source, "__", datevar_string)

            )
        
        # for calendar_month, change date format to month on day 1 for proper processing by concatMAU()
        if (datevar_type == "calendar_month") data <- data %>%
            dplyr::mutate(datevar = lubridate::ymd(paste0(datevar, "-01")))
        
        names(data)[names(data) == "idvar"] <- idvar
        
    }
    
    data
    
}

## Concatenate MAU source names and source date variable names.
concatMAU <- function(
    data, # object from identifyMAUrows() objects concatenated via dplyr::bind_rows()
    datevar = "datevar",
    mau_source = "mau_source",
    mau_datevar = "mau_datevar"
) {
    
    data[mau_source] <- apply(
        X = data[names(data)[grep(mau_source, names(data))]],
        MARGIN = 1,
        FUN = paste0, collapse = ", "
    )
    names(data)[names(data) == mau_source] <- "mau_source"
    data <- data %>%
        dplyr::mutate(mau_source = mau_source %>% removeNAFromString())
    names(data)[names(data) == "mau_source"] <- mau_source
    data <- data[names(data)[!grepl(paste0(mau_source, "."), names(data))]]
    
    data[mau_datevar] <- apply(
        X = data[names(data)[grep(mau_datevar, names(data))]],
        MARGIN = 1,
        FUN = paste0, collapse = ", "
    )
    names(data)[names(data) == mau_datevar] <- "mau_datevar"
    data <- data %>%
        dplyr::mutate(mau_datevar = mau_datevar %>% removeNAFromString())
    names(data)[names(data) == "mau_datevar"] <- mau_datevar
    data <- data[names(data)[!grepl(paste0(mau_datevar, "."), names(data))]]
    
    names(data)[names(data) == datevar] <- "datevar"
    data <- data %>%
        dplyr::mutate(calendar_month = format(as.Date(datevar), "%Y-%m"))
    names(data)[names(data) == "datevar"] <- datevar
    
    data
    
}



# Make table footnote summarizing participant A1c actual month of measurement.
makeA1cFootnote <- function(
    data,
    variable,
    baseline_or_mo3
) {
    
    names(data)[names(data) == variable] <- "makeA1cFootnote_variable"
    
    if (baseline_or_mo3 == "baseline") {
        
        tally_include_in_analysis <- tally_mo0$include_in_analysis %>% setNames(tally_mo0$mo0_actual_month_from_enrollment_completed)
        descriptor <- "Baseline"
        
    }
    if (baseline_or_mo3 == "mo3") {
        
        tally_include_in_analysis <- tally_mo3$include_in_analysis %>% setNames(tally_mo3$mo3_actual_month_from_enrollment_completed)
        descriptor <- "Month-3"
        
    }
    
    footnote_data_valid <- data %>%
        dplyr::filter(makeA1cFootnote_variable %in% names(tally_include_in_analysis[tally_include_in_analysis == TRUE])) %>%
        dplyr::distinct(

            makeA1cFootnote_variable,
            identifier

        ) %>%
        dplyr::group_by(makeA1cFootnote_variable) %>%
        dplyr::tally() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n %>% as.character())
    footnote_data_valid <- footnote_data_valid %>%
        dplyr::arrange(makeA1cFootnote_variable) %>%
        dplyr::mutate(
            
            makeA1cFootnote_variable = dplyr::case_when(
                length(footnote_data_valid$makeA1cFootnote_variable) == 1 ~ makeA1cFootnote_variable,
                TRUE ~ ifelse(
                    dplyr::row_number() == dplyr::n(),
                    paste0(
                        "and ",
                        makeA1cFootnote_variable
                    ),
                    makeA1cFootnote_variable
                )
            ),
            n = dplyr::case_when(
                length(footnote_data_valid$n) == 1 ~ n,
                TRUE ~ ifelse(
                    dplyr::row_number() == dplyr::n(),
                    paste0(
                        "and ",
                        n
                    ),
                    n
                )
            )
            
        )

    footnote_data_invalid <- data %>%
        dplyr::filter(makeA1cFootnote_variable %in% names(tally_include_in_analysis[tally_include_in_analysis == FALSE])) %>%
        dplyr::distinct(

            makeA1cFootnote_variable,
            identifier

        ) %>%
        dplyr::group_by(makeA1cFootnote_variable) %>%
        dplyr::tally() %>%
        dplyr::ungroup() %>%
        dplyr::mutate(n = n %>% as.character())
    footnote_data_invalid <- footnote_data_invalid %>%
        dplyr::arrange(makeA1cFootnote_variable) %>%
        dplyr::mutate(
            
            makeA1cFootnote_variable = dplyr::case_when(
                length(footnote_data_invalid$makeA1cFootnote_variable) == 1 ~ makeA1cFootnote_variable,
                TRUE ~ ifelse(
                    dplyr::row_number() == dplyr::n(),
                    paste0(
                        "and ",
                        makeA1cFootnote_variable
                    ),
                    makeA1cFootnote_variable
                )
            ),
            n = dplyr::case_when(
                length(footnote_data_invalid$n) == 1 ~ n,
                TRUE ~ ifelse(
                    dplyr::row_number() == dplyr::n(),
                    paste0(
                        "and ",
                        n
                    ),
                    n
                )
            )
            
        )
    
    plural_valid <- "s"
    respectively_valid <- ", respectively"
    if (length(footnote_data_valid$n) == 1) {
        
        if (as.numeric(footnote_data_valid$n) == 1) plural_valid <- ""
        respectively_valid <- ""
        
    }
    footnote_valid <- paste0(
        descriptor,
        " values analyzed: ",
        ifelse(
            length(footnote_data_valid$n) == 1,
            footnote_data_valid$n,
            ifelse(
                2 < length(footnote_data_valid$n),
                paste0(
                    footnote_data_valid$n,
                    collapse = ", "
                ),
                paste0(
                    footnote_data_valid$n,
                    collapse = " "
                )
            )
        ),
        " A1c value",
        plural_valid,
        " (with timestamp",
        plural_valid,
        ") reported ",
        ifelse(
            2 < length(footnote_data_valid$makeA1cFootnote_variable),
            paste0(
                footnote_data_valid$makeA1cFootnote_variable,
                collapse = ", "
            ),
            paste0(
                footnote_data_valid$makeA1cFootnote_variable,
                collapse = " "
            )
        ),
        " months from enrollment",
        respectively_valid,
        "; A1c ",
        tolower(descriptor),
        " values without timestamps were also analyzed (i.e., assumed to be valid)."
    )

    plural_invalid <- "s"
    respectively_invalid <- ", respectively."
    if (length(footnote_data_invalid$n) == 1) {
        
        if (as.numeric(footnote_data_invalid$n) == 1) plural_invalid <- ""
        respectively_invalid <- "."
        
    }
    footnote_invalid <- paste0(
        descriptor,
        " values not analyzed: ",
        ifelse(
            length(footnote_data_invalid$n) == 1,
            footnote_data_invalid$n,
            ifelse(
                2 < length(footnote_data_invalid$n),
                paste0(
                    footnote_data_invalid$n,
                    collapse = ", "
                ),
                paste0(
                    footnote_data_invalid$n,
                    collapse = " "
                )
            )
        ),
        " A1c value",
        plural_invalid,
        " (with timestamp",
        plural_invalid,
        ") reported ",
        ifelse(
            2 < length(footnote_data_invalid$makeA1cFootnote_variable),
            paste0(
                footnote_data_invalid$makeA1cFootnote_variable,
                collapse = ", "
            ),
            paste0(
                footnote_data_invalid$makeA1cFootnote_variable,
                collapse = " "
            )
        ),
        " months from enrollment",
        respectively_invalid
    )
    
    footnote <- paste(
        footnote_valid,
        footnote_invalid
    )
    
    footnote
    
}



# SAP Section 7.4 Exploratory Objectives: Filter partner data values to include
# only analysis population participants.
filterValuesPartnerData <- function(
    data,
    population_data = analysis_population,
    filter_mode = "participants", # "participants" if used at end of notebook; "dates" if used earlier
        # to drop dates outside of per-participant study period window (e.g., 90 days)
    datevar = NULL # required if filter_mode = "dates"
) {
    
    if (filter_mode == "dates") {
        
        output <- data %>%
            dplyr::inner_join(
                y = population_data %>%
                    dplyr::rename(participant_id = lifescan_id) %>%
                    dplyr::distinct(

                        participant_id,
                        enrollment_completed

                    ),
                by = "participant_id"
            )
        output <- output[!is.na(output[datevar]),]
        output <- output %>%
            completeAllDaysAndWeeks(
                identifier = "participant_id",
                study_start_datevar = "enrollment_completed",
                datevar = datevar
            )
        output <- output$output_with_input %>%
            dplyr::filter(
                actual_response == TRUE,
                in_study_period_raw == TRUE
            ) %>%
            dplyr::select(dplyr::all_of(names(data)))
        
        data <- output
        
    }
    
    if (filter_mode == "participants") {
        
        data <- data %>%
            dplyr::rename(lifescan_id = participant_id) %>%
            dplyr::inner_join(
                y = population_data %>%
                    dplyr::distinct(

                        identifier,
                        lifescan_id,
                        branch_name

                    ),
                by = "lifescan_id"
            ) %>%
            dplyr::rename(arm = branch_name) %>%
            print_dataset_dims(idvar = "lifescan_id")

        print(paste0("unique(data$arm) = ", unique(data$arm)))
        
    }
    
    data
    
}



# BMI formula
calculateBMI <- function(
    data,
    weight_var,
    height_var,
    units = "lbs_inches", # possible values: "lbs_inches", "kg_cm"
    bmi_varname = "bmi"
) {
    
    if (units == "lbs_inches") data$calculateBMI_bmi <- data[[weight_var]] / (data[[height_var]]^2) * 703
    if (units == "kg_cm") data$calculateBMI_bmi <- data[[weight_var]] / (data[[height_var]]^2)
    if (!is.null(bmi_varname)) names(data)[names(data) == "calculateBMI_bmi"] <- bmi_varname
    data
}