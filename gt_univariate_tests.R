# Funtion that uses gtsummary to conduct univariate tests of
# a continuous outcome with every variable in a set of predictor
# variables.
gt_univariate_tests <- function(
  datain, # input dataset
  outcome_var, # continuous outcome variable
  predictor_vars # set of predictor variables to marginally test
) {
  for (predictor_var in predictor_vars) {
    
    if(datain[[predictor_var]] %>% class != "numeric") gtsum_out <- datain %>%
        
        # Variables to keep. Exclude identifiers so we don't summarize them.
        dplyr::select(all_of(predictor_var), all_of(outcome_var)) %>%
        
        gtsummary::tbl_summary(
          
          by = predictor_var,
          
          type = all_continuous() ~ "continuous2",
          statistic = list(
            #             all_continuous() ~ c("{N_nonmiss}", "{N_miss}", "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
            all_continuous() ~ c("{N_nonmiss}", "{mean} ({sd})", "{median} ({p25}, {p75})", "{min}, {max}"),
            all_categorical() ~ "{n} ({p})"
          ),
          digits = list(
            #             all_continuous() ~ c(0, 0, c(1, 2), c(1, 0, 0), c(0, 0)),
            all_continuous() ~ c(0, c(1, 2), c(1, 0, 0), c(0, 0)),
            all_categorical() ~ c(0, 1)
          )
          
        ) %>%
        gtsummary::add_n() %>% # add column with total number of non-missing observations
        gtsummary::add_p() %>% # test for a difference between groups
        #     add_p(test = all_categorical() ~ "chisq.test") %>% # test for a difference between groups; default is fisher.test; see https://www.danieldsjoberg.com/gtsummary/reference/add_p.tbl_summary.html
        #     add_q() %>% # multiplicity-adjusted
        gtsummary::bold_labels() %>%
        gtsummary::modify_table_styling(
          
          #         columns = stat_0,
          align = "right"
          
          #     ) %>%
        )
    
    if(datain[[predictor_var]] %>% class == "numeric") gtsum_out <- datain %>%
        
        # Variables to keep. Exclude identifiers so we don't summarize them.
        dplyr::select(all_of(predictor_var), all_of(outcome_var)) %>%
        
        lm(formula = datain[[outcome_var]] ~ datain[[predictor_var]]) %>%
        gtsummary::tbl_regression()
    
    if (predictor_var == predictor_vars[1]) {
      
      tbl_predictor_vars <- dplyr::tibble(predictor_var = predictor_var)
      list_gtsum_out <- list(gtsum_out = gtsum_out)
      #             names(list_gtsum_out)[names(list_gtsum_out) == "gtsum_out"] <- predictor_var
      for (name_gtsum in names(list_gtsum_out)[grepl("gtsum_out", names(list_gtsum_out))]) {
        names(list_gtsum_out)[names(list_gtsum_out) == name_gtsum] <- stringr::str_replace(names(list_gtsum_out)[names(list_gtsum_out) == name_gtsum], "gtsum_out", predictor_var)
      }
      rm(name_gtsum)
      
    }
    if (predictor_var != predictor_vars[1]) {
      
      tbl_predictor_vars <- tbl_predictor_vars %>% dplyr::bind_rows(dplyr::tibble(predictor_var = all_of(predictor_var)))
      list_gtsum_out_new <- list(gtsum_out = gtsum_out)
      #             names(list_gtsum_out)[names(list_gtsum_out) == "gtsum_out"] <- predictor_var
      for (name_gtsum in names(list_gtsum_out_new)[grepl("gtsum_out", names(list_gtsum_out_new))]) {
        names(list_gtsum_out_new)[names(list_gtsum_out_new) == name_gtsum] <- stringr::str_replace(names(list_gtsum_out_new)[names(list_gtsum_out_new) == name_gtsum], "gtsum_out", predictor_var)
      }
      rm(name_gtsum)
      list_gtsum_out <- c(
        list_gtsum_out,
        list_gtsum_out_new
      )
      
    }
    
  }
  return(
    list(
      tbl_predictor_vars = tbl_predictor_vars,
      list_gtsum_out = list_gtsum_out
    )
  )
  
}