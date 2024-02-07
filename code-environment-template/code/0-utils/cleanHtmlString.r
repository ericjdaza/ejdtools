# Function to remove HTML tags and anchors. Copied verbatim from: https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
cleanHtmlString <- function(htmlString) { 
    
    return(gsub("<.*?>", "", htmlString))
    
}