# fancyTable: Table with percentages
fancyTable <- function(
  datain,
  varnames,
  round_digits = 2,
  margin_value = 2 # 1 = row percentages, 2 = col percentages
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
  mat_out %>% knitr::kable("simple")
  
}