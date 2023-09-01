# Function to attach confidence intervals (CIs) to gtsummary::tbl_summary() output object
# with gtsummary::add_ci() implemented.
gtsummaryAttachCIs <- function(
    data, # must be the "table_body" object contained in a gtsummary::tbl_summary() output object; e.g., tbl_summary_output_object$table_body
    variables_needing_cis, # variables needing confidence intervals (CIs) attached
    label_mean, # label denoting mean outcome, possibly with another statistics like the standard deviation (SD)
    ci_prefix = "(95% CI: ", # prefix for display; e.g., "(CI: ", "("; overridden with "(" if attachment_method = "bind_rows"
    attachment_method = "paste0", # = "paste0" to concatenate to label_mean; "bind_rows" to add as row below label_mean
    paste_collapse = "\n", # collapse argument for paste0 if attachment_method = "paste0"
    ci_label = "95% CI" # confidence interval label if attachment_method = "bind_rows"
) {
    
    stat_numbers <- names(data$table_body)[
        grepl("^stat_", names(data$table_body)) &
        !grepl("^stat_label$", names(data$table_body))
    ] %>%
        stringr::str_replace("stat_", "") %>%
        as.numeric()
    
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
               attachment_method == "paste0" ~ label_mean,
               attachment_method == "bind_rows" ~ ci_label
            ),
            row_type = "level" # used if attachment_method = "bind_rows"
            
        )
    if (attachment_method == "paste0") data_ci_stats <- data_ci_stats %>%
        dplyr::select(-c(
            var_type,
            var_label,
            row_type
        ))
    if (attachment_method == "bind_rows") names(data_ci_stats)[names(data_ci_stats) %in% paste0("ci_stat_", stat_numbers)] <-
        gsub("ci_", "", names(data_ci_stats)[names(data_ci_stats) %in% paste0("ci_stat_", stat_numbers)])
    
    if (attachment_method == "paste0") for (stat_number in 1:max(stat_numbers)) {
        
        data_with_ci_stats <- data_stats %>%
            dplyr::full_join(
                y = data_ci_stats,
                by = c(
                    "label",
                    "stat_label",
                    "variable"
                )
            )
        data_with_ci_stats[
            data_with_ci_stats$variable %in% variables_needing_cis &
            data_with_ci_stats$label == label_mean,
            paste0("stat_", stat_number)
        ] <- apply(
            X = data_with_ci_stats[
                data_with_ci_stats$variable %in% variables_needing_cis &
                data_with_ci_stats$label == label_mean,
                c(
                    paste0("stat_", stat_number),
                    paste0("ci_stat_", stat_number)
                )
            ],
            MARGIN = 1,
            FUN = paste, collapse = paste_collapse
        )
        data_with_ci_stats <- data_with_ci_stats %>%
            dplyr::select(-paste0("ci_stat_", stat_numbers))
        
    }
    
    if (attachment_method == "bind_rows") {
        
        data_variables <- unique(data$table_body$variable)
        for (data_variable in data_variables) {

            if (data_variable == data_variables[1]) {

                # print("gtsummaryAttachCIs: Entering the if block == 1")
                if (!(data_variable %in% variables_needing_cis)) data_with_ci_stats <- data_stats %>%
                    dplyr::filter(variable == data_variable)
                if (data_variable %in% variables_needing_cis) data_with_ci_stats <- data_stats %>%
                    dplyr::filter(variable == data_variable) %>%
                    dplyr::filter(
                        
                        1 <= dplyr::row_number(),
                        dplyr::row_number() <= min(grep(grepCorrection(label_mean), data_stats$label))
                        
                    ) %>%
                    dplyr::bind_rows(data_ci_stats %>% dplyr::filter(variable == data_variable)) %>%
                    dplyr::bind_rows(
                        data_stats %>%
                            dplyr::filter(variable == data_variable) %>%
                            dplyr::filter(min(grep(grepCorrection(label_mean), data_stats$label)) < dplyr::row_number())
                    )
                # print(data_with_ci_stats)

            }
            if (data_variable != data_variables[1]) {

                # print("gtsummaryAttachCIs: Entering if block >1")
                if (!(data_variable %in% variables_needing_cis)) data_with_ci_stats <- data_with_ci_stats %>%
                    dplyr::bind_rows(data_stats %>% dplyr::filter(variable == data_variable))
                if (data_variable %in% variables_needing_cis) data_with_ci_stats <- data_with_ci_stats %>%
                    dplyr::bind_rows(
                        data_stats %>%
                            dplyr::filter(variable == data_variable) %>%
                            dplyr::filter(
                                
                                1 <= dplyr::row_number(),
                                dplyr::row_number() <= min(grep(grepCorrection(label_mean), data_stats$label))                                
                                
                            ) %>%
                            dplyr::bind_rows(data_ci_stats %>% dplyr::filter(variable == data_variable)) %>%
                            dplyr::bind_rows(
                                data_stats %>%
                                    dplyr::filter(variable == data_variable) %>%
                                    dplyr::filter(min(grep(grepCorrection(label_mean), data_stats$label)) < dplyr::row_number())
                            )
                    )
                # print(data_with_ci_stats)

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