# +
# Prints the characteristics of a dataset in a pipe (i.e., returns that dataset for storage in a new dataset).
print_dataset_dims <- function(
    data, # input dataset
    idvar = NA, # participant ID
    return_dataset = TRUE # set to FALSE if you only want to print the characteristics
){

    rows <- data %>% nrow()
    cols <- data %>% ncol()
    drows <- data %>% distinct %>% nrow()
    didrows <- NA
    if (!is.na(idvar)) didrows <- data %>% dplyr::select({idvar}) %>% dplyr::distinct() %>% nrow()
    
    glue_string <- "Dimensions (rows, cols): ({rows}, {cols})\nTotal distinct rows: {drows}"
    if (!is.na(idvar)) glue_string <- paste0(
        glue_string,
        "\nTotal distinct {idvar} values: {didrows}"
    )
    
    glue(
        glue_string,
        rows = rows,
        cols = cols,
        drows = drows,
        didrows = didrows
    ) %>% print
    
    if (return_dataset == TRUE) return(data)
    
}
