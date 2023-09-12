# +
# Checks all unique observed values of a set of categorical variables.
check_categorical_values <- function(
    data, # input dataset
    variables_to_check = NA
){

    for (varname in variables_to_check) {

        if (varname == variables_to_check[1]) check_responses <- dplyr::tibble(
            varname = varname,
            unique = data[[varname]] %>% unique()
        )
        if (varname != variables_to_check[1]) check_responses <- check_responses %>%
            dplyr::bind_rows(
                dplyr::tibble(
                    varname = varname,
                    unique = data[[varname]] %>% unique()
                )
            )

    }
    check_responses %>%
        dplyr::mutate(observed_response = TRUE) %>%
        dplyr::arrange(varname, unique) %>%
        tidyr::pivot_wider(
            id_cols = varname,
            names_from = unique,
            values_from = observed_response
        ) %>%
        return()
    
}