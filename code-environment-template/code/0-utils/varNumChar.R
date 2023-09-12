## varNumChar: Identify numeric or character variables
varNumChar <- function(
  datain,
  char_types = c("character", "factor", "logical")
) {
  
  return(
    list(
      vars_num = names(datain)[!(lapply(datain, function(x) class(x)) %>% cbind %in% char_types)],
      vars_char = names(datain)[lapply(datain, function(x) class(x)) %>% cbind %in% char_types]
    )
  )
  
}
# # examples:
# 
# vars_num <- varNumChar(my_dataset)$vars_num
# vars_num %>% cbind %>% print
# for (name in vars_num) {
#   name %>% print
#   my_dataset[name] %>% as.matrix %>% summary %>% knitr::kable() %>% print
# }
# rm(name)
# 
# vars_char <- varNumChar(my_dataset)$vars_char
# vars_char %>% cbind %>% print
# for (name in vars_char) {
#   name %>% print
#   cbind(
#     my_dataset[[name]] %>% as.character %>% table(useNA = "always"),
#     my_dataset[[name]] %>% as.character %>% table(useNA = "always") %>% prop.table * 100 %>% round(digits = 2)
#   ) %>% knitr::kable() %>% print()
# }
# rm(name)