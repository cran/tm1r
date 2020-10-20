tm1_run_chore <- function(tm1_connection, chore = "") {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  chore <- gsub(" ", "%20", chore, fixed=TRUE)

  u6 <- "api/v1/Chores('"
  u7 <- chore
  u8 <- "')/tm1.Execute"

  # url development
  url <- paste0(tm1_base_url, u6, u7, u8)
  #url = "https://localhost:8881/api/v1/Chores('create_Y2Ksales_cube')/tm1.Execute"

  # post request
  tm1_chore_return <-
    httr::POST(url,
               httr::add_headers("Authorization" = tm1_auth_key),
               httr::add_headers("Content-Type" = "application/json"))



  # return manipulation
  # if content is empty; then success
  # else get the error message to differentiate abortion and minor error
  if(httr::content(tm1_chore_return, "text", encoding = "UTF-8") == "")
  {
    tm1_chore_message <- "ChoreCompletedSuccessfully"
    print(tm1_chore_message)
    }
  else
  {
    # check return if error
    if (is.null(jsonlite::fromJSON(httr::content(tm1_chore_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_chore_return, "text"))$error$message)
      stop()
    }

  }




}
