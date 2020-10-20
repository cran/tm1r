tm1_delete_view <- function(tm1_connection,
                            cube, view) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  cube <- gsub(" ", "%20", cube, fixed=TRUE)
  view <- gsub(" ", "%20", view, fixed=TRUE)

  u6 <- "api/v1/Cubes('"
  u7 <- cube
  u8 <- "')/Views('"
  u9 <- view
  u10 <- "')"

  # url development
  url <- paste0(tm1_base_url, u6, u7, u8, u9, u10)
  #url = "https://localhost:8881/api/v1/Cubes('SalesCube')/Views('TempViewByMDX')"


  # delete request
  tm1_process_return <-
    httr::DELETE(url,
         httr::add_headers("Authorization" = tm1_auth_key),
         httr::add_headers("Content-Type" = "application/json"))

  # return manipulation
  # if content is empty; then success
  # else get the error message to differentiate abortion and minor error
  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
  {
    tm1_process_message <- "ViewDeletedSuccessfully"
    print(tm1_process_message)

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
