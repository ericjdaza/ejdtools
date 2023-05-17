# +
# Capitalize all words in all elements of a data frame.
# Function made using Examples in https://rdrr.io/cran/R.utils/man/capitalize.html.
capitalize_all_words <- function(variable) {
    
    capitalize_all_words_in_phrase <- function(phrase) {
        
        if (!anyNA(phrase)) {
            words <- strsplit(phrase, " ")[[1]]
            return(paste(R.utils::capitalize(words), collapse = " "))
        }
        if (anyNA(phrase)) return(NA)
        
    }
    apply(
        X = variable %>% as.data.frame(),
        MARGIN = 1,
        FUN = capitalize_all_words_in_phrase
    )
    
}