replaceAll <- function(
  useSpark = FALSE, # process in Spark or RStudio (default; i.e., TRUE)
  datain,
  colnames_to_process = NULL,
  toreplace_num = NULL,
  toreplace_char = NULL,
  replacewith_num = NA,
  replacewith_char = NA
) {
  
  # # For debugging only.
  # rm(
  #   useSpark,
  #   datain,
  #   colnames_to_process,
  #   toreplace_num,
  #   toreplace_char,
  #   replacewith_num,
  #   replacewith_char
  # )
  # 
  # # datain <- tbl_touse[1:100,]
  # # colnames_to_process <- names(tbl_touse)
  # # toreplace_num <- -1
  # # toreplace_char <- "-1"
  # # replacewith_num <- NA
  # # replacewith_char <- NA
  # # str(datain)
  # 
  # useSpark = FALSE
  # datain = tbl_3m_admissions
  # colnames_to_process = vars_attributes
  # toreplace_num = -1
  # toreplace_char = "-1"
  # replacewith_num = NA
  # replacewith_char = NA
  
  if (is.null(colnames_to_process)) colnames_to_process <- names(datain)
  if (useSpark == FALSE) {
    
    df_touse <- as.data.frame(datain)
    for (colname in colnames_to_process) {
      
      if (!is.null(toreplace_char) & class(df_touse[[colname]]) %in% c("character", "factor")) ifelse(
        class(df_touse[[colname]]) == "character",
        df_touse[[colname]] <- replace(
          x = df_touse[[colname]],
          list = grep(toreplace_char, df_touse[[colname]]),
          values = rep(replacewith_char, length(grep(toreplace_char, df_touse[[colname]])))
        ),
        df_touse[[colname]] <- as.factor(
          replace(
            x = as.character(df_touse[[colname]]),
            list = grep(toreplace_char, as.character(df_touse[[colname]])),
            values = rep(replacewith_char, length(grep(toreplace_char, as.character(df_touse[[colname]]))))
          )
        )
      )
      if (!is.null(toreplace_num) & class(df_touse[[colname]]) %in% c("numeric", "integer")) df_touse[[colname]] <- replace(
        x = df_touse[[colname]],
        list = grep(toreplace_num, df_touse[[colname]]),
        values = rep(replacewith_num, length(grep(toreplace_num, df_touse[[colname]])))
      )

    }
    return(dplyr::as_tibble(df_touse))
    
  }
  if (useSpark == TRUE) {
    
    df_touse <- datain
    tbl_types <- sparklyr::sdf_schema(df_touse) %>%
      lapply(function(x) do.call(dplyr::tibble, x)) %>% # adapted from https://campus.datacamp.com/courses/introduction-to-spark-in-r-using-sparklyr/going-native-use-the-native-interface-to-manipulate-spark-dataframes?ex=10
      dplyr::bind_rows() %>%
      dplyr::filter(name %in% colnames_to_process)
    for (colname in colnames_to_process) {
      
      if (
        !is.null(toreplace_char) &
        tbl_types %>%
          dplyr::filter(name == colname) %>%
          dplyr::select(type) == "StringType"
      ) df_touse <- df_touse %>%
        dplyr::mutate(
          colname = replace(
            x = df_touse %>% dplyr::select(colname),
            list = grep(toreplace_char, df_touse %>% dplyr::select(colname)),
            values = rep(replacewith_char, length(grep(toreplace_char, df_touse %>% dplyr::select(colname))))
          )
        )
      if (
        !is.null(toreplace_num) &
        tbl_types %>%
        dplyr::filter(name == colname) %>%
        dplyr::select(type) %in% c(
          "DoubleType",
          "IntegerType",
          "LongType"
        )
      ) df_touse <- df_touse %>%
        dplyr::mutate(
          colname = replace(
            x = df_touse %>% dplyr::select(colname),
            list = grep(toreplace_num, df_touse %>% dplyr::select(colname)),
            values = rep(replacewith_num, length(grep(toreplace_num, df_touse %>% dplyr::select(colname))))
          )
        )

    }
    return(df_touse)
    
  }
  
}
