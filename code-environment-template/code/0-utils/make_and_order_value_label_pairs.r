# Function to assign variable labels to values in the order desired for output.
make_and_order_value_label_pairs <- function(
    var_values, # list of variable values in order desired
    var_labels # list of variable labels in order desired
) {    
    setNames(as.list(var_labels), var_values)
}
