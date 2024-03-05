# Common ggplot UDFs.
ggplotUDFs <- function(
    data,
    # type = "jitter", # "jitter", "scatter"
    x_variable = NA, # variable name
    y_variable, # variable name
    jitter_seed = 20240216 # random seed for jitter plot
) {
    
    ggplot_data <- data
    if (!is.na(x_variable)) names(ggplot_data)[names(ggplot_data) == x_variable] <- "x_variable"
    names(ggplot_data)[names(ggplot_data) == y_variable] <- "y_variable"
    
    # Jitter plot and summaries.
    if (is.na(x_variable)) {
        
        set.seed(jitter_seed)
        ggplot_output <- ggplot_data %>%
            ggplot2::ggplot(
                aes(
                    x = 0,
                    y = y_variable
                )
            ) +
            ggplot2::geom_boxplot(alpha = 1.0) +
            ggplot2::geom_violin(alpha = 0.6) +
            ggplot2::geom_jitter(
                height = 0,
                alpha = 0.3
            )
        
    }
    
    # Scatter plot.
    if (!is.na(x_variable)) {
        
        ggplot_output <- ggplot_data %>%
            ggplot2::ggplot(
                aes(
                    x = x_variable,
                    y = y_variable
                )
            ) +
            ggplot2::geom_point(alpha = 0.3) +
            ggplot2::xlab(x_variable)
        
    }
    
    ggplot_output <- ggplot_output +
        ggplot2::theme_classic() +
        ggplot2::ylab(y_variable)
    
    return(ggplot_output)
    
}