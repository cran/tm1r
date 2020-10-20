tm1_get_dimension_attributes <- function(tm1_connection, dimension) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)

  u6 <- "api/v1/Dimensions('"
  u7 <- dimension
  u8 <- "')/Hierarchies('"
  u9 <- dimension
  u10 <- "')/ElementAttributes"

  # url development
  url <- paste0(tm1_base_url, u6, u7, u8, u9, u10)
  #url = "https://localhost:8881/api/v1/Dimensions('Account1')/Hierarchies('Account1')/ElementAttributes

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))

  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
    stop()
  }

  # make it proper
  tm1_dim_attrs <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$value

  #change to data frame
  tm1_dim_attrs <- as.data.frame(tm1_dim_attrs)

  #return
  return(tm1_dim_attrs)



}
