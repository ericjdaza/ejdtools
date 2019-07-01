ejdtools_charvar <- function(
  datain,
  datain_name = NA,
  varname,
  vardomaingroup = NA,
  vardomain = NA
) {
  
  # # Debugging code only.
  # rm(
  #   datain,
  #   varname
  # )
  # datain <- tbl_touse_to_na_neg1
  # varname <- names(tbl_touse_to_na_neg1)[6]
  
  names(datain)[which(names(datain)==varname)] <- "sumVarChars_varname"
  if (class(datain$sumVarChars_varname) == "factor") datain$sumVarChars_varname <- as.character(datain$sumVarChars_varname)
  
  Class <- class(datain$sumVarChars_varname)
  n_obs <- sum(!is.na(datain$sumVarChars_varname))
  p_obs <- sum(!is.na(datain$sumVarChars_varname)) / length(datain$sumVarChars_varname) * 100
  n_miss <- sum(is.na(datain$sumVarChars_varname))
  p_miss <- sum(is.na(datain$sumVarChars_varname)) / length(datain$sumVarChars_varname) * 100
  n_obs_unique <- NA
  n5_obs <- NA
  p5_obs <- NA
  Minimum <- NA
  Maximum <- NA
  Median <- NA
  Mean <- NA
  `Interquartile Range` <- NA
  `Standard Deviation` <- NA
  if (Class == "character") {
    
    table_sorted <- datain %>%
      dplyr::filter(!is.na(sumVarChars_varname)) %>%
      dplyr::count(sumVarChars_varname) %>%
      dplyr::arrange(desc(n))
    n_obs_unique <- length(unique(datain$sumVarChars_varname))
    n5_obs <- paste(
      head(table_sorted, 5)$sumVarChars_varname,
      collapse = ", "
    )
    p5_obs <- paste(
      round(
        x = head(table_sorted, 5)$n / sum(!is.na(datain$sumVarChars_varname)) * 100,
        digits = 2
      ),
      collapse = ", "
    )
    
  }
  if (!(Class %in% c("character", "factor"))) {
    
    sumVarChars_varname_touse <- as.numeric(datain$sumVarChars_varname)
    if (sum(is.na(sumVarChars_varname_touse)) != length(datain$sumVarChars_varname)) {
      
      Minimum <- min(sumVarChars_varname_touse, na.rm = TRUE)
      Maximum <- max(sumVarChars_varname_touse, na.rm = TRUE)
      Median <- median(sumVarChars_varname_touse, na.rm = TRUE)
      Mean <- mean(sumVarChars_varname_touse, na.rm = TRUE)
      `Interquartile Range` <- IQR(sumVarChars_varname_touse, na.rm = TRUE)
      `Standard Deviation` <- sd(sumVarChars_varname_touse, na.rm = TRUE)
      
    }
    if (Class == "Date") {
      
      Minimum <- as.Date(x = Minimum, origin = as.Date("1970-01-01"))
      Maximum <- as.Date(x = Maximum, origin = as.Date("1970-01-01"))
      Median <- as.Date(x = Median, origin = as.Date("1970-01-01"))
      Mean <- as.Date(x = Mean, origin = as.Date("1970-01-01"))
      
    }
    
  }
  tbl_out <- dplyr::tibble(
    Variable = varname,
    Domain = vardomaingroup,
    Subdomain = vardomain,
    Class = Class,
    `Number of Observed Values (not NA)` = n_obs,
    `Percentage of Observed Values (not NA)` = p_obs,
    `Number of Missing Values (NA)` = n_miss,
    `Percentage of Missing Values (NA)` = p_miss,
    `Number of Unique Observed Values` = n_obs_unique,
    `Five Most Common Observed Values` = n5_obs,
    `Frequencies (%) of Five Most Common Observed Values` = p5_obs,
    Minimum = as.character(Minimum),
    Maximum = as.character(Maximum),
    Median = as.character(Median),
    Mean = as.character(Mean),
    `Interquartile Range` = `Interquartile Range`,
    `Standard Deviation` = `Standard Deviation`
  )
  if (!is.na(datain_name)) tbl_out <- dplyr::bind_cols(
    x = dplyr::tibble(Dataset = datain_name),
    tbl_out
  )
  tbl_out
  
}
