tm1_delete_subset <- function(tm1_connection,
                            dimension,
                            subset) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)
  subset <- gsub(" ", "%20", subset, fixed=TRUE)

  u6 <- "api/v1/Dimensions('"
  u7 <- dimension
  u8 <- "')/Hierarchies('"
  u9 <- dimension
  u10 <- "')/Subsets('"
  u11 <- subset
  u12 <- "')"


  # url development
  url <- paste0(tm1_base_url, u6, u7, u8, u9, u10, u11, u12)
  #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Subsets('test')"

  # delete request
  tm1_process_return <-
    httr::DELETE(url,
         httr::add_headers("Authorization" = tm1_auth_key),
         httr::add_headers("Content-Type" = "application/json"))

  # return manipulation

  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
    {

    }
  else
    {
    # check return if error
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
        }

     }

}
