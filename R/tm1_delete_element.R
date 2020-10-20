tm1_delete_element <- function(tm1_connection,
                            dimension,
                            element,
                            parent = "") {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)
  element <- gsub(" ", "%20", element, fixed=TRUE)
  parent <- gsub(" ", "%20", parent, fixed=TRUE)

  ##########################################3
  ### if no parent, delete element

  if (parent == "") {

  u6 <- "api/v1/Dimensions('"
  u7 <- dimension
  u8 <- "')/Hierarchies('"
  u9 <- dimension
  u10 <- "')/Elements('"
  u11 <- element
  u12 <- "')"


  # url development
  url <- paste0(tm1_base_url, u6, u7, u8, u9, u10, u11, u12)
  #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Elements('test')"

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

  ##########################################
  ### if parent, only delete component


  else {

    u6 <- "api/v1/Dimensions('"
    u7 <- dimension
    u8 <- "')/Hierarchies('"
    u9 <- dimension
    u10 <- "')/Edges(ParentName='"
    u11 <- parent
    u12 <- "',ComponentName='"
    u13 <- element
    u14 <- "')"


    # url development
    url <- paste0(tm1_base_url, u6, u7, u8, u9, u10, u11, u12, u13, u14)
    #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/
    #Edges(ParentName='TheActualParent'sName', ComponentName='TheActualComponent'sName')"

    # post request
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




}
