# Create a dataset with each variable's marginal importance and evidence for
# differentiating a binary variable.
# 
# Definitions:
# 
#   marginal importance = the standardized difference between the two groups
#   statistical evidence = 1 - p-value (i.e., statistical significance)

impevi_binout <- function(
  datain # the `_data` object output by gtsummary::tbl_summary() %>% gtsummary::as_gt()
) {
  
  
  # Calculate evidence.
  tbl_evidence <- datain %>%
    dplyr::filter(!is.na(p.value)) %>%
    dplyr::distinct(variable, p.value) %>%
    dplyr::mutate(
      
      statistical_evidence_pvalue_flipped = 1 - p.value,
      statistical_evidence_neglog_pvalue = -log(p.value, 10),
      
      plt05 = (p.value < 0.05),
      plt01 = (p.value < 0.01),
      plt001 = (p.value < 0.001),
      
      statsig_bonferroni = (p.adjust(p = p.value, method = "bonferroni") < 0.05),
      statsig_fdr = (p.adjust(p = p.value, method = "fdr") < 0.05)
      
    )
  
  
  # Get sample size per group for importance calculations.
  
  ## Continuous predictor variables
  tbl_sample_sizes_continuous <- datain %>%
    dplyr::filter(
      var_type == "continuous2",
      label == "N",
      !is.na(stat_1),
      !is.na(stat_2)
    ) %>%
    dplyr::distinct(
      variable,
      stat_1,
      stat_2
    ) %>%
    dplyr::mutate(
      
      n1 = stat_1 %>% stringr::str_replace_all(",", "") %>% as.numeric,
      n2 = stat_2 %>% stringr::str_replace_all(",", "") %>% as.numeric
      
    )
  
  ## Categorical predictor variables
  tbl_sample_sizes_categorical <- datain %>%
    dplyr::filter(
      var_type == "categorical",
      row_type == "level",
      !is.na(stat_1),
      !is.na(stat_2)
    ) %>%
    dplyr::distinct(
      variable,
      var_type,
      stat_1,
      stat_2
    ) %>%
    dplyr::mutate(
      
      n1 = NA,
      n2 = NA
      
    )
  
  # gregexpr code source: https://stackoverflow.com/questions/14249562/find-the-location-of-a-character-in-string
  for (i in 1:nrow(tbl_sample_sizes_categorical)) {
    
    tbl_sample_sizes_categorical$n1[i] <- tbl_sample_sizes_categorical$stat_1[i] %>%
      trimws %>%
      stringr::str_replace_all(",", "") %>%
      substr(
        1,
        unlist(
          gregexpr(
            pattern = "\\(",
            tbl_sample_sizes_categorical$stat_1[i] %>%
              trimws %>%
              stringr::str_replace_all(",", "")
          )[1]
        ) - 1
      ) %>%
      as.numeric
    
    tbl_sample_sizes_categorical$n2[i] <- tbl_sample_sizes_categorical$stat_2[i] %>%
      trimws %>%
      stringr::str_replace_all(",", "") %>%
      substr(
        1,
        unlist(
          gregexpr(
            pattern = "\\(",
            tbl_sample_sizes_categorical$stat_2[i] %>%
              trimws %>%
              stringr::str_replace_all(",", "")
          )[1]
        ) - 1
      ) %>%
      as.numeric
    
  }
  rm(i)
  
  
  # Calculate importance per predictor variable type.
  
  ## Continuous predictor variables
  
  ## Create standardized U statistics for wilcox.test, based on "maximum = (number enrolled) *
  ## (number not enrolled)" for each test.
  ## (reference: https://sphweb.bumc.bu.edu/otlt/mph-modules/bs/bs704_nonparametric/bs704_nonparametric4.html)
  ## 
  ## We want larger standardized U values to reflect more meaningful differences. But larger
  ## raw U statistics indicate less meaningful differences. The minimum possible U statistic is
  ## 0, which indicates "complete separation between groups" (see bu.edu reference above).
  ## Hence, we will first standardize the U statistic by dividing it by its maximum possible
  ## value (see bu.edu reference above). We will then subtract this from 1. This way,
  ## standardized U values will indicate the most meaningful difference when they equal 1,
  ## with smaller values (bounded below by 0) indicating smaller differences.
  
  tbl_importance_continuous <- datain %>%
    dplyr::filter(
      test_name == "wilcox.test",
      !is.na(statistic)
    ) %>%
    dplyr::distinct(
      variable,
      var_type,
      statistic
    ) %>%
    dplyr::left_join(
      
      y = tbl_sample_sizes_continuous %>%
        dplyr::mutate(Umax = n1 * n2),
      by = "variable"
      
    ) %>%
    dplyr::mutate(Ustandardized = 1 - statistic / Umax) %>%
    dplyr::rename(Ustatistic = statistic)
  
  ## Categorical predictor variables
  
  ## Use comp.prop (https://rdrr.io/cran/StatMatch/man/comp.prop.html) to calculate four common
  ## similarity/metrics that have the nice property of ranging from 0 to 1. We will transform
  ## them such that 0 indicates similarity, and 1 indicates dissimilarity.
  
  # Calculate the similarity/dissimilarity metrics. Do not include "overlap",
  # as this is just 1 - tvd.
  
  tbl_importance_categorical <- tbl_sample_sizes_categorical %>%
    dplyr::distinct(variable, var_type) %>%
    dplyr::mutate(
      
      tvd = NA,
      Bhatt_flipped = NA,
      Hell = NA,
      
      compprop_median = NA,
      compprop_mean = NA
      
    )
  
  for (i in 1:nrow(tbl_importance_categorical)) {
    
    tbl_temp_compprop <- tbl_sample_sizes_categorical %>%
      dplyr::filter(variable == tbl_importance_categorical$variable[i])
    
    compprop_out <- StatMatch::comp.prop(
      p1 = tbl_temp_compprop$n1 / sum(tbl_temp_compprop$n1),
      p2 = tbl_temp_compprop$n2 / sum(tbl_temp_compprop$n2),
      n1 = sum(tbl_temp_compprop$n1),
      n2 = sum(tbl_temp_compprop$n2)
    )
    
    tbl_importance_categorical$tvd[i] <- compprop_out$meas[1]
    tbl_importance_categorical$Bhatt_flipped[i] <- 1 - compprop_out$meas[3]
    tbl_importance_categorical$Hell[i] <- compprop_out$meas[4]
    
    tbl_importance_categorical$compprop_median[i] <- median(c(
      tbl_importance_categorical$tvd[i],
      tbl_importance_categorical$Bhatt_flipped[i],
      tbl_importance_categorical$Hell[i]
    ))
    tbl_importance_categorical$compprop_mean[i] <- mean(c(
      tbl_importance_categorical$tvd[i],
      tbl_importance_categorical$Bhatt_flipped[i],
      tbl_importance_categorical$Hell[i]
    ))
    
  } 
  rm(i)
  
  
  # Combine all evidence and importance values.
  tbl_main <- tbl_importance_continuous %>%
    dplyr::select(variable, var_type, Ustandardized) %>%
    dplyr::bind_rows(tbl_importance_categorical) %>%
    dplyr::full_join(
      y = tbl_evidence,
      by = "variable"
    ) %>%
    dplyr::mutate(
      marginal_importance = case_when(
        var_type == "continuous2" ~ Ustandardized,
        var_type == "categorical" ~ compprop_median
      )
    ) %>%
    dplyr::arrange(dplyr::desc(statistical_evidence_pvalue_flipped))
  
  
  # Return stuff.
  list(
    
    tbl_main = tbl_main,
    tbl_importance_categorical = tbl_importance_categorical,
    tbl_importance_continuous = tbl_importance_continuous,
    tbl_sample_sizes_categorical = tbl_sample_sizes_categorical,
    tbl_sample_sizes_continuous = tbl_sample_sizes_continuous,
    tbl_evidence = tbl_evidence
    
  ) %>% return()
  
}