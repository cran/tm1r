tm1_get_cubes <- function(tm1_connection, ShowControlObjects = FALSE) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  u6 <- "api/v1/Cubes"

  if (ShowControlObjects == FALSE) {
    u7 <- "?$filter=not%20startswith(Name,'}')"
  } else{
    u7 <- ""
  }

  # url development
  url <- paste0(tm1_base_url, u6, u7)
  #url = "https://localhost:8881/api/v1/Cubes"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))


  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
  }
  else
  {
    tm1_return <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))
    return(tm1_return$value$Name)

  }







}
