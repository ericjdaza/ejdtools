# Function to attach confidence intervals (CIs) to gtsummary::tbl_summary() output object
# with gtsummary::add_ci() implemented.
# 
# NOTE: The variables being summarized must all have the same var_type value in the
# "table_body" object contained in a gtsummary::tbl_summary() output object; i.e.,
# "continuous" (or containing that string; e.g., "continuous2"), "categorical", or
# "dichotomous."
gtsummaryAttachCIs <- function(
    data, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    variables_needing_cis, # variables needing confidence intervals (CIs) attached
    label_mean_or_frequency, # label denoting variable mean or frequency; corresponds to {mean} (continuous variables) and {n} (dichotomous variables)
        # in the "statistics" argument of gtsummary::tbl_summary()
    ci_prefix = "(95% CI: ", # prefix for display; e.g., "(CI: ", "("; overridden with "(" if attachment_method = "bind_rows"
    attachment_method = "bind_rows", # = "paste0" to concatenate to label_mean_or_frequency; "bind_rows" to add as row below label_mean_or_frequency
    paste_collapse = "\n", # collapse argument for paste0 if attachment_method = "paste0"
    ci_label = "95% CI" # confidence interval label if attachment_method = "bind_rows"
) {
    
    stat_numbers <- names(data$table_body)[
        grepl("^stat_", names(data$table_body)) &
        !grepl("^stat_label$", names(data$table_body))
    ] %>%
        stringr::str_replace("stat_", "") %>%
        as.numeric()
    print("gtsummaryAttachCIs: stat_numbers =")
    print(stat_numbers)
    
    data_stats <- data$table_body %>%
        dplyr::select(-paste0("ci_stat_", stat_numbers))
    ci_prefix <- ifelse(
        attachment_method == "bind_rows",
        "(",
        ci_prefix
    )
    data_ci_stats <- data$table_body %>%
        dplyr::filter(
            
            row_type == "label",
            variable %in% variables_needing_cis 
            
        ) %>%
        dplyr::select(-paste0("stat_", stat_numbers)) %>%
        dplyr::distinct() %>%
        dplyr::mutate_at(
            vars(dplyr::starts_with("ci_stat_")),
            function(x) paste0(ci_prefix, x, ")")
        ) %>%
        dplyr::mutate(
            label = dplyr::case_when(
                
                attachment_method == "paste0" & length(grep("continuous", unique(data$table_body$var_type))) > 0 ~ label_mean_or_frequency,
                attachment_method == "bind_rows" ~ ci_label,
                TRUE ~ label
                
            )
        )
    if (attachment_method == "bind_rows") data_ci_stats <- data_ci_stats %>%
        dplyr::mutate(
            
            row_type = "level",
            stat_label = NA
            
        )
    if (attachment_method == "paste0") data_ci_stats <- data_ci_stats %>%
        dplyr::select(-c(
            var_type,
            var_label,
            row_type
        ))
    if (attachment_method == "bind_rows") names(data_ci_stats)[names(data_ci_stats) %in% paste0("ci_stat_", stat_numbers)] <-
        gsub("ci_", "", names(data_ci_stats)[names(data_ci_stats) %in% paste0("ci_stat_", stat_numbers)])
    
    if (attachment_method == "paste0") {
        
        print("gtsummaryAttachCIs: Starting if block 1")
        
        if (length(grep("continuous", unique(data$table_body$var_type))) > 0) {
            
            print("gtsummaryAttachCIs: Starting if block 1.1")
            data_with_ci_stats <- data_stats %>%
                dplyr::full_join(
                    y = data_ci_stats,
                    by = c(
                        "variable",
                        "label",
                        "stat_label"
                    )
                )
            data_with_ci_stats_label_or_stat_label <- data_with_ci_stats$label
            print("gtsummaryAttachCIs: Ending if block 1.1")
            
        }
        if (unique(data$table_body$var_type) == "dichotomous") {
            
            print("gtsummaryAttachCIs: Starting if block 1.2")
            data_with_ci_stats <- data$table_body %>%
                dplyr::mutate_at(
                    vars(dplyr::starts_with("ci_stat_")),
                    function(x) paste0(ci_prefix, x, ")")
                )
            data_with_ci_stats_label_or_stat_label <- data_with_ci_stats$stat_label
            print("gtsummaryAttachCIs: Ending if block 1.2")
            
        }
        for (stat_number in 1:max(stat_numbers)) {
            
            print(paste0("gtsummaryAttachCIs: If block 1: stat_number = ", stat_number))
            data_with_ci_stats[
                data_with_ci_stats$variable %in% variables_needing_cis &
                !is.na(data_with_ci_stats_label_or_stat_label) &
                data_with_ci_stats_label_or_stat_label == label_mean_or_frequency,
                paste0("stat_", stat_number)
            ] <- apply(
                X = data_with_ci_stats[
                    data_with_ci_stats$variable %in% variables_needing_cis &
                    !is.na(data_with_ci_stats_label_or_stat_label) &
                    data_with_ci_stats_label_or_stat_label == label_mean_or_frequency,
                    c(
                        paste0("stat_", stat_number),
                        paste0("ci_stat_", stat_number)
                    )
                ],
                MARGIN = 1,
                FUN = paste, collapse = paste_collapse
            )
            
        }
        data_with_ci_stats <- data_with_ci_stats %>%
            dplyr::select(-paste0("ci_stat_", stat_numbers))
        
        print("gtsummaryAttachCIs: Ending if block 1")
        
    }
    
    if (attachment_method == "bind_rows") {
        
        data_variables <- unique(data$table_body$variable)
        if (length(grep("continuous", unique(data$table_body$var_type))) > 0) data_stats_label_or_stats_label <- data_stats$label
        if (unique(data$table_body$var_type) == "dichotomous") data_stats_label_or_stats_label <- data_stats$stat_label
        for (data_variable in data_variables) {

            if (data_variable == data_variables[1]) {
                
                print(paste0("gtsummaryAttachCIs: Starting if block 2: data_variable = ", data_variable))
                if (!(data_variable %in% variables_needing_cis)) data_with_ci_stats <- data_stats %>%
                    dplyr::filter(variable == data_variable)
                if (data_variable %in% variables_needing_cis) data_with_ci_stats <- data_stats %>%
                    dplyr::filter(variable == data_variable) %>%
                    dplyr::filter(
                        
                        1 <= dplyr::row_number(),
                        dplyr::row_number() <= min(grep(grepCorrection(label_mean_or_frequency), data_stats_label_or_stats_label))
                        
                    ) %>%
                    dplyr::bind_rows(data_ci_stats %>% dplyr::filter(variable == data_variable)) %>%
                    dplyr::bind_rows(
                        data_stats %>%
                            dplyr::filter(variable == data_variable) %>%
                            dplyr::filter(min(grep(grepCorrection(label_mean_or_frequency), data_stats_label_or_stats_label)) < dplyr::row_number())
                    )
                print(paste0("gtsummaryAttachCIs: Ending if block 2: data_variable = ", data_variable))

            }
            if (data_variable != data_variables[1]) {

                print(paste0("gtsummaryAttachCIs: Starting if block 3: data_variable = ", data_variable))
                if (!(data_variable %in% variables_needing_cis)) data_with_ci_stats <- data_with_ci_stats %>%
                    dplyr::bind_rows(data_stats %>% dplyr::filter(variable == data_variable))
                if (data_variable %in% variables_needing_cis) data_with_ci_stats <- data_with_ci_stats %>%
                    dplyr::bind_rows(
                        data_stats %>%
                            dplyr::filter(variable == data_variable) %>%
                            dplyr::filter(
                                
                                1 <= dplyr::row_number(),
                                dplyr::row_number() <= min(grep(grepCorrection(label_mean_or_frequency), data_stats_label_or_stats_label))                                
                                
                            ) %>%
                            dplyr::bind_rows(data_ci_stats %>% dplyr::filter(variable == data_variable)) %>%
                            dplyr::bind_rows(
                                data_stats %>%
                                    dplyr::filter(variable == data_variable) %>%
                                    dplyr::filter(min(grep(grepCorrection(label_mean_or_frequency), data_stats_label_or_stats_label)) < dplyr::row_number())
                            )
                    )
                print(paste0("gtsummaryAttachCIs: Ending if block 3: data_variable = ", data_variable))

            }

        }
        
    }
    
    data_new <- data
    data_new$table_body <- data_with_ci_stats
    data_new$table_styling$header <- data_new$table_styling$header %>%
        dplyr::filter(!grepl("ci_stat_", column)) %>%
        dplyr::mutate(label = gsub(grepCorrection(" (**95% CI**)"), "", label))
    data_new$table_styling$cols_merge <- data_new$table_styling$cols_merge %>%
        dplyr::filter(!grepl("ci_stat_", rows))
    
    list(
        data_stats = data_stats,
        data_ci_stats = data_ci_stats,
        data_with_ci_stats = data_with_ci_stats,
        data_new = data_new
    )
    
}