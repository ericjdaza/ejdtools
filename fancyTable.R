# fancyTable: Table with percentages
fancyTable <- function(
  datain,
  varnames,
  round_digits = 2,
  margin_value = 2 # 1 = row percentages, 2 = col percentages
) {
  
  n <- table(datain[varnames], useNA = "ifany")
  if (length(varnames) == 1) return(
    cbind(
      n,
      (n %>% prop.table * 100) %>% round(round_digits) %>% paste0("%")
    ) %>% knitr::kable("simple")
  )
  if (length(varnames) == 2) return(
    cbind(
      n,
      (n %>% prop.table(margin = margin_value) * 100) %>% round(round_digits) %>% paste0("%") %>% matrix(ncol=ncol(n))
    ) %>% knitr::kable("simple")
  )
  
}