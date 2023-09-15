# Mask variable values such as study participant identifiers.
maskValues <- function(
    data,
    variable,
    masked_variable = NULL, # name the new masked variable; can be
        # original variable name; if set to NULL, "_masked" is appended to original
        # variable name
    keep_variable = FALSE # set to TRUE to keep original variable; not advised
        # because this defeats the purpose of masking this variable's values
) {
    
    mask_identifier <- seq(1, length(unique(data[[variable]]))) %>% setNames(unique(data[[variable]]))
    
    names(data)[names(data) == variable] <- "maskValues_variable"
    data <- data %>% dplyr::mutate(maskValues_masked_variable = mask_identifier[maskValues_variable] %>% as.character())
    names(data)[names(data) == "maskValues_masked_variable"] <- paste0(variable, "_masked")
    if (!is.null(masked_variable)) names(data)[names(data) == paste0(variable, "_masked")] <- masked_variable
    
    if (keep_variable == TRUE) names(data)[names(data) == "maskValues_variable"] <- variable
    if (keep_variable == FALSE) data$maskValues_variable <- NULL
    
    data
    
}