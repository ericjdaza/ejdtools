fancyTable <- function(
  datain,
  varnames,
  round_digits = 2,
  margin_value = 2, # 1 = row percentages, 2 = col percentages
  kable_print = TRUE,
  kable_type = NA
) {
  
  n <- table(datain[varnames], useNA = "ifany")
  if (length(varnames) == 1) {
    
    tbl_out <- dplyr::tibble(
      n,
      pct = (n %>% prop.table * 100) %>% round(round_digits)
    )
    if (margin_value == 1) names(tbl_out)[names(tbl_out) == "pct"] <- "row %"
    if (margin_value == 2) names(tbl_out)[names(tbl_out) == "pct"] <- "col %"
    
  }
  if (length(varnames) == 2) tbl_out <- dplyr::tibble(
    n,
    pct = (n %>% prop.table(margin = margin_value) * 100) %>% round(round_digits)
  )
  mat_out <- tbl_out %>% as.matrix
  rownames(mat_out) <- n %>% rownames
  if (kable_print == TRUE) {
    
    ifelse(
      !is.na(kable_type),
      print_out <- mat_out %>% knitr::kable(kable_type),
      print_out <- mat_out %>% knitr::kable()
    )
    print_out
    
  }
  if (kable_print == FALSE) return(
    list(
      tbl_out = tbl_out,
      mat_out = mat_out
    )
  )
  
}