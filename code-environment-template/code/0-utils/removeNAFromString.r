# Remove NA from string.
removeNAFromString <- function(
    string,
    delimiter = ",",
    delimiter_space = " "
) {
    
    for (variation in c(
        paste0("NA", delimiter, delimiter_space),
        paste0("NA ", delimiter, delimiter_space),
        paste0(delimiter, delimiter_space, "NA"),
        paste0(delimiter, delimiter_space, "NA ")
    )) string <- gsub(
        variation,
        "",
        string
    )
    
    string
    
}