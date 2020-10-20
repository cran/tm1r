tm1_logout <- function(tm1_connection) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  u6 <- "api/logout"

  # url development
  url <- paste0(tm1_base_url, u6)
  #url = "https://localhost:8881/api/v1/Configuration"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))


}
