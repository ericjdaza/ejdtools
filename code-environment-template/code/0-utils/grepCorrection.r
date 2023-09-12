# Function to clean up text for use in grep(), grepl(), etc.
grepCorrection <- function(text_string) {
    
    # for (reg_exp in c("(", ")", "?", ":")) if (grepl(paste0("\\", reg_exp), text_string) == TRUE) text_string <- gsub(
    for (reg_exp in c("(", ")", "?", ":", "*", "%", " ")) if (grepl(paste0("\\", reg_exp), text_string) == TRUE) text_string <- gsub(
        paste0("\\", reg_exp),
        paste0("\\\\", reg_exp),
        text_string
    )
    return(text_string)
    
}