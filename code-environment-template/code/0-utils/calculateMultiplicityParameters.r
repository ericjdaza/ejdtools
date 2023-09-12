# Function to set parameters for multiple testing (multiplicity) adjustment of inferential
# statistics (i.e., p-values, confidence intervals, etc.).
calculateMultiplicityParameters <- function(
    familywise_alpha = 0.05, # familywise statistical significance level; i.e., maximum false positive probability
    path_and_datasets # all datasets (and their paths) with number of tests saved as "tests = {n}"
        # e.g., c("/Users/myname/myfolder/dataset1.csv", "/Users/myname/myfolder/dataset2.csv")
) {
    
    for (dataset in path_and_datasets) {
        
        if (dataset == path_and_datasets[1]) datasets_and_tests <- dplyr::tibble(

            dataset = dataset,
            tests = readr::read_csv(dataset, show_col_types = FALSE)$tests

        )
        if (dataset != path_and_datasets[1]) datasets_and_tests <- datasets_and_tests %>%
            dplyr::bind_rows(
                dplyr::tibble(

                    dataset = dataset,
                    tests = readr::read_csv(dataset, show_col_types = FALSE)$tests

                )
            )
        
    }
    rm(dataset)
    total_number_of_tests <- sum(datasets_and_tests$tests)
    bonferroni_alpha_per_test <- familywise_alpha / total_number_of_tests
    confidence_level_per_test <- 1 - bonferroni_alpha_per_test
    
    list(
        datasets_and_tests = datasets_and_tests,
        familywise_alpha = familywise_alpha,
        total_number_of_tests = total_number_of_tests,
        bonferroni_alpha_per_test = bonferroni_alpha_per_test,
        confidence_level_per_test = confidence_level_per_test
    )
    
}