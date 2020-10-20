tm1_get_dimension_subsets <- function(tm1_connection, dimension) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)

  u6 <- "api/v1/Dimensions('"
  u7 <- dimension
  u8 <- "')/Hierarchies('"
  u9 <- dimension
  u10 <- "')/Subsets"

  # url development
  url <- paste0(tm1_base_url, u6, u7, u8, u9, u10)
  #url = "https://localhost:8881/api/v1/Dimensions('Account1')/Hierarchies('Account1')/Subsets"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))

  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
    stop()
  }

  if (length(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$value) == 0) {
    message(paste0("No subset found in dimension ", dimension))
    stop()
  }

  tm1_return <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))

  return_object <- data.frame(tm1_return$value$Name, tm1_return$value$UniqueName, tm1_return$value$Expression)
  colnames(return_object)[1] <- "Name"
  colnames(return_object)[2] <- "UniqueName"
  colnames(return_object)[3] <- "Expression"

  return(return_object)




}
