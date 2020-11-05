## Create function to one-hot-decode.
oneHotDecode <- function(
  datain,
  onehot_zero_is_NA = FALSE,
  catnames, # one-hot variables
  catname_affixes = NA # text to remove from newvar output
) {
  
  # # For debugging only.
  # rm(
  #   datain,
  #   onehot_zero_is_NA,
  #   catnames,
  #   catname_affixes
  # )
  # datain = tbl_ejd_liger_target_pop[1:10,]
  # onehot_zero_is_NA = TRUE
  # catnames = c("aiq_gender_female", "aiq_gender_male")
  # catname_affixes = NA
  
  # Replace NA onehot_zero_is_NA with 0.
  if (onehot_zero_is_NA == TRUE) for (catname in catnames) datain[catname][is.na(datain[catname])] <- 0
  
  # Generate new variable.
  newvar <- as.character(
    apply(
      X = datain[catnames],
      MARGIN = 1,
      FUN = function(x) names(x)[as.logical(x)]
    )
  )
  
  # Replace empty values with NA.
  newvar[newvar == "character(0)"] <- NA
  
  # If applicable, remove catname_prefix.
  if (!anyNA(catname_affixes)) {
    for (catname_prefix in catname_affixes) newvar <- gsub(
      pattern = catname_prefix,
      replacement = "",
      x = newvar
    )
  }
  
  # Return values.
  newvar
  
}