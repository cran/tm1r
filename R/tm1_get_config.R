tm1_get_config <- function(tm1_connection) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # url development
  u6 <- "api/v1/Configuration"
  url <- paste0(tm1_base_url, u6)
  #url = "https://localhost:8881/api/v1/Configuration"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))

  # make it proper
  tm1_config <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))

  #change to data frame
  tm1_config <- as.data.frame(as.matrix(tm1_config))

  # change column name from random
  colnames(tm1_config)[1] <- "Value"

  #delete 1st row
  tm1_config <- tm1_config[-c(1), ]

  #change to proper data frame again
  tm1_config <- as.data.frame(as.matrix(tm1_config))
  colnames(tm1_config)[1] <- "Value"

  #return
  return(tm1_config)



}
