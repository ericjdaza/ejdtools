# Wrapper function for gtsummary::tbl_summary() and common modifications.
gtsummaryWrapper <- function(
    data,
    group = NA, # appears as columns named "stat_..." in table_body object of tbl_summary() output
    type_list = NULL, # to force tbl_summary() to apply variable types as specified
    statistic_all_continuous = "{median} ({p25}, {p75})", # tbl_summary() default
    statistic_all_categorical = "{n} ({p}%)", # tbl_summary() default
    digits_all_continuous = NULL, # use to specify digits in tbl_summary(); must be specified together with digits_all_categorical
    digits_all_categorical = NULL, # use to specify digits in tbl_summary(); must be specified together with digits_all_continuous
    missing = "ifany", # tbl_summary() default; other allowed values: "no", "always"
    missing_text = "Unknown", # tbl_summary() default; other examples: "Missing (n(%))"
    add_overall_column = FALSE, # use to add column with overall summary statistics
    location = "row", # add_stat_label() default; other allowed values: "column"
    label_all_continuous = NULL, # use to specify digits in add_stat_label(); must be specified together with label_all_categorical
    label_all_categorical = NULL, # use to specify digits in add_stat_label(); must be specified together with label_all_continuous
    add_p_method = NA, # use to add p-values via add_p(); other allowed values: "default", "fisher_simulated"
    add_ci_method = NA, # use to add confidence intervals via add_ci(); other allowed values: "default", "percent"
    relocate_c = NULL, # use to re-order column names of table_body object of tbl_summary() output
    bold_labels = TRUE, # set to TRUE to apply bold_labels()
    align = NA, # modify_column_alignment() default if NA; other allowed values: "left", "right", "center"
    bold_levels = FALSE, # set to TRUE to apply bold_levels()
    label_caption = NA # argument for modify_caption()
) {
    
    if (!is.na(group)) {
        
        if (!is.null(type_list)) {

            if (!is.null(digits_all_continuous) & !is.null(digits_all_categorical)) {
                
                print("gtsummaryWrapper: Starting if block 1")
                gtsummary_out <- data %>%
                    gtsummary::tbl_summary(

                        by = {group},
                        type = type_list,
                        statistic = list(
                            all_continuous() ~ statistic_all_continuous,
                            all_categorical() ~ statistic_all_categorical
                        ),
                        digits = list(
                            all_continuous() ~ digits_all_continuous,
                            all_categorical() ~ digits_all_categorical
                        ),
                        missing = missing,
                        missing_text = missing_text

                    )
                print("gtsummaryWrapper: Ending if block 1")
                
            }
            if (is.null(digits_all_continuous) & is.null(digits_all_categorical)) {
                
                print("gtsummaryWrapper: Starting if block 2")
                gtsummary_out <- data %>%
                    gtsummary::tbl_summary(

                        by = {group},
                        type = type_list,
                        statistic = list(
                            all_continuous() ~ statistic_all_continuous,
                            all_categorical() ~ statistic_all_categorical
                        ),
                        missing = missing,
                        missing_text = missing_text

                    )
                print("gtsummaryWrapper: Ending if block 2")
                
            }
            
        }

        if (is.null(type_list)) {

            if (!is.null(digits_all_continuous) & !is.null(digits_all_categorical)) {
                
                print("gtsummaryWrapper: Starting if block 3")
                gtsummary_out <- data %>%
                    gtsummary::tbl_summary(

                        by = {group},
                        type = all_continuous() ~ "continuous2",
                        statistic = list(
                            all_continuous() ~ statistic_all_continuous,
                            all_categorical() ~ statistic_all_categorical
                        ),
                        digits = list(
                            all_continuous() ~ digits_all_continuous,
                            all_categorical() ~ digits_all_categorical
                        ),
                        missing = missing,
                        missing_text = missing_text

                    )
                print("gtsummaryWrapper: Ending if block 3")
                
            }
            if (is.null(digits_all_continuous) & is.null(digits_all_categorical)) {
                
                print("gtsummaryWrapper: Starting if block 4")
                gtsummary_out <- data %>%
                    gtsummary::tbl_summary(

                        by = {group},
                        type = all_continuous() ~ "continuous2",
                        statistic = list(
                            all_continuous() ~ statistic_all_continuous,
                            all_categorical() ~ statistic_all_categorical
                        ),
                        missing = missing,
                        missing_text = missing_text

                    )
                print("gtsummaryWrapper: Ending if block 4")
                
            }
            
        }
        
    }
    
    if (is.na(group)) {
        
        if (!is.null(type_list)) {

            if (!is.null(digits_all_continuous) & !is.null(digits_all_categorical)) {
                
                print("gtsummaryWrapper: Starting if block 5")
                gtsummary_out <- data %>%
                    gtsummary::tbl_summary(

                        type = type_list,
                        statistic = list(
                            all_continuous() ~ statistic_all_continuous,
                            all_categorical() ~ statistic_all_categorical
                        ),
                        digits = list(
                            all_continuous() ~ digits_all_continuous,
                            all_categorical() ~ digits_all_categorical
                        ),
                        missing = missing,
                        missing_text = missing_text

                    )
                print("gtsummaryWrapper: Ending if block 5")
                
            }
            if (is.null(digits_all_continuous) & is.null(digits_all_categorical)) {
                
                print("gtsummaryWrapper: Starting if block 6")
                gtsummary_out <- data %>%
                    gtsummary::tbl_summary(

                        type = type_list,
                        statistic = list(
                            all_continuous() ~ statistic_all_continuous,
                            all_categorical() ~ statistic_all_categorical
                        ),
                        missing = missing,
                        missing_text = missing_text

                    )
                print("gtsummaryWrapper: Ending if block 6")
                
            }
            
        }

        if (is.null(type_list)) {

            if (!is.null(digits_all_continuous) & !is.null(digits_all_categorical)) {
                
                print("gtsummaryWrapper: Starting if block 7")
                gtsummary_out <- data %>%
                    gtsummary::tbl_summary(

                        type = all_continuous() ~ "continuous2",
                        statistic = list(
                            all_continuous() ~ statistic_all_continuous,
                            all_categorical() ~ statistic_all_categorical
                        ),
                        digits = list(
                            all_continuous() ~ digits_all_continuous,
                            all_categorical() ~ digits_all_categorical
                        ),
                        missing = missing,
                        missing_text = missing_text

                    )
                print("gtsummaryWrapper: Ending if block 7")
                
            }
            if (is.null(digits_all_continuous) & is.null(digits_all_categorical)) {
                
                print("gtsummaryWrapper: Starting if block 8")
                gtsummary_out <- data %>%
                    gtsummary::tbl_summary(

                        type = all_continuous() ~ "continuous2",
                        statistic = list(
                            all_continuous() ~ statistic_all_continuous,
                            all_categorical() ~ statistic_all_categorical
                        ),
                        missing = missing,
                        missing_text = missing_text

                    )
                print("gtsummaryWrapper: Ending if block 8")
                
            }
            
        }
        
    }
    
    # Update column header; see gtsum_out$table_styling.
    gtsummary_out <- gtsummary_out %>% gtsummary::modify_header(all_stat_cols() ~ "**{level} (N={n})**")
    
    # Add column with overall summary statistics (https://www.danieldsjoberg.com/gtsummary/reference/add_overall.html)
    # Not needed if all original rows were bound to the original data with group variable value set to "Overall", and
    # then input into this function in the "data" argument.
    if (add_overall_column == TRUE) gtsummary_out <- gtsummary_out %>% gtsummary::add_overall(col_label = "**Overall (N={N})**")

    # Specify statistic labels.
    if (!is.null(label_all_continuous) & !is.null(label_all_categorical)) {
        
        print("gtsummaryWrapper: Starting if block 9")
        gtsummary_out <- gtsummary_out %>%
            gtsummary::add_stat_label(

                location = location,
                label = list(
                    all_continuous() ~ label_all_continuous,
                    all_categorical() ~ label_all_categorical
                )

            )
        print("gtsummaryWrapper: Ending if block 9")
        
    }
    if (is.null(label_all_continuous) & is.null(label_all_categorical)) {
        
        print("gtsummaryWrapper: Starting if block 10")
        gtsummary_out <- gtsummary_out %>% gtsummary::add_stat_label(location = location)
        print("gtsummaryWrapper: Ending if block 10")
        
    }
    
    # Add p-values.
    if (!is.na(add_p_method)) {
        
        print("gtsummaryWrapper: Starting if block 11")
        if (add_p_method == "default") gtsummary_out <- gtsummary_out %>% gtsummary::add_p()
        if (add_p_method == "fisher_simulated") gtsummary_out <- gtsummary_out %>% gtsummary::add_p()
            gtsummary::add_p( # source: https://stackoverflow.com/questions/61360954/error-in-add-p-for-variable-x-and-test-fisher-test-p-value-omitted
                test.args = all_tests("fisher.test") ~ list(
                    simulate.p.value = TRUE,
                    B = 100000
                )
            )
        print("gtsummaryWrapper: Ending if block 11")
        
    }
    
    # Add confidence intervals (CIs).
    if (!is.na(add_ci_method)) {
        
        print("gtsummaryWrapper: Starting if block 12")
        if (add_ci_method == "default") gtsummary_out <- gtsummary_out %>% gtsummary::add_ci()
        if (add_ci_method == "percent") gtsummary_out <- gtsummary_out %>% gtsummary::add_ci(
                statistic = list(all_continuous() ~ "{conf.low}%, {conf.high}%")
                # , pattern = "{stat} ({ci})"
            )
        print("gtsummaryWrapper: Ending if block 12")
        
    }
    
    # Re-order columns; see gtsum_out$table_body.
    if (!is.null(relocate_c)) {
        
        print("gtsummaryWrapper: Starting if block 13")
        gtsummary_out <- gtsummary_out %>%
            gtsummary::modify_table_body(

                ~ .x %>%
                dplyr::relocate({relocate_c})

            )
        print("gtsummaryWrapper: Ending if block 13")
        
    }
    
    # Update column alignment; see gtsum_out$table_styling.
    if (!is.na(align)) gtsummary_out <- gtsummary_out %>%
        gtsummary::modify_column_alignment(

            columns = tidyr::everything(),
            align = align

        )
    
    # Bold the label and level values.
    if (bold_labels == TRUE) gtsummary_out <- gtsummary_out %>% gtsummary::bold_labels()
    if (bold_levels == TRUE) gtsummary_out <- gtsummary_out %>% gtsummary::bold_levels()
    
    # Add the caption.
    if (!is.na(label_caption)) gtsummary_out <- gtsummary_out %>% gtsummary::modify_caption(label_caption)
    
    # Return final object.
    gtsummary_out

}