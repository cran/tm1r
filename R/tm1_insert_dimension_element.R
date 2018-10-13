tm1_insert_dimension_element <- function(tm1_connection,
                            dimension,
                            element,
                            parent = "" ) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Dimensions('"
  u6 <- dimension
  u7 <- "')/Hierarchies('"
  u8 <- dimension
  u9 <- "')/Elements"

  if (parent == "")
    {
      u10 <- ""
    }
  else
    {
      parent <- gsub(" ", "%20", parent, fixed=TRUE)
      u10 <- paste( "('", parent, "')/Components" , sep="")

    }

  # url development
  url <- paste(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, sep = "")
  #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Elements"

  # sample parameter syntax
  parameters <-
  "{
    \"Name\": \"New Element\"
  }"

  elementbody1 <- "{    \"Name\": \""
  elementbody2 <- element
  elementbody3 <-" \" }"



  elementbody <- paste(elementbody1, elementbody2, elementbody3, sep = "")

  # post request
  tm1_process_return <-
    httr::POST(url,
         httr::add_headers("Authorization" = tm1_auth_key),
         httr::add_headers("Content-Type" = "application/json"),
         body = elementbody)

  # return manipulation

    # check return if error
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
    }





}
