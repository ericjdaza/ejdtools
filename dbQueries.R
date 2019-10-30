dbQueries <- function(
  query_type, # "CreateAppendTable", "ReadTable", "SendQuery", "GetQuery", "justRemoveTable"
  dbname_touse, # database name
  host_touse = "", # IP address
  port_touse = NA, # port number
  username = "",
  password = NULL,
  datain,
  table_name,
  query_statement,
  remove_table = FALSE
) {
  
  con <- RPostgres::dbConnect(
    RPostgres::dbDriver("Postgres"),
    user = username,
    password = ifelse(
      !is.null(password),
      password,
      rstudioapi::askForPassword("Password, please. Ahrm.")
    ),
    dbname = dbname_touse,
    host = host_touse,
    port = port_touse,
    sslmode = 'require'
  )
  if (query_type == "CreateAppendTable") {
    
    if (remove_table == TRUE) RPostgres::dbRemoveTable(
      conn = con,
      name = table_name
    )
    RPostgres::dbCreateTable(
      conn = con,
      name = table_name,
      fields = datain
    )
    RPostgres::dbAppendTable(
      conn = con,
      name = table_name,
      value = datain
    )
    RPostgres::dbDisconnect(con)
    
  }
  if (query_type == "ReadTable") {
    
    return(
      RPostgres::dbReadTable(
        conn = con,
        name = table_name
      )
    )
    RPostgres::dbDisconnect(con)
    
  }
  if (query_type == "SendQuery") {
    
    RPostgres::dbSendQuery(
      conn = con,
      statement = query_statement
    )
    RPostgres::dbDisconnect(con)
    
  }
  if (query_type == "GetQuery") {
    
    dataout <- RPostgres::dbGetQuery(
      conn = con,
      statement = query_statement
    )
    RPostgres::dbDisconnect(con)
    if ("data.table" %in% (.packages())) {
      ## coerce result to data.table
      ## if that package is loaded
      dataout <- as.data.table(dataout)
    }
    return(dplyr::as_tibble(dataout))
    
  }
  if (query_type == "justRemoveTable") {
    
    RPostgres::dbRemoveTable(
      conn = con,
      name = table_name
    )
    RPostgres::dbDisconnect(con)
    
  }
  
}
