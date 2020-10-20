tm1_get_element <- function(tm1_connection, dimension, element = "", index = 0) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)
  element <- gsub(" ", "%20", element, fixed=TRUE)

  u6 <- "api/v1/Dimensions('"
  u7 <- dimension
  u8 <- "')/Hierarchies('"
  u9 <- dimension
  u10 <- "')/Elements"

  if (element != "") {
    u11 <- "('"
    u12 <- element
    u13 <- "')?$expand=Components"
  }
  else
  {
    if (index > 0)
  {
    u11 <- "?$filter=Index%20eq%20"
    u12 <- index
    u13 <- ""
    }
  else
  {
    #error
    message("element or index parameter should be specified AND index should be greater than 0")
    stop()

  }
  }

  # url development
  url <- paste0(tm1_base_url, u6, u7, u8, u9, u10, u11, u12, u13)
  #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Elements('Year')?$expand=Components"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))

  tm1_element_object <- list(name=c(NULL), uniquename=c(NULL), type=c(NULL),
                             level=c(NULL), index=c(0), components=c(NULL), attributes=c(NULL))

  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    return(tm1_element_object)
  }
  else
  {
    if (element != "") {
      content <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))
    }
    else
    {
      content <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$value
    }

    tm1_element_object$name <- content$Name
    tm1_element_object$uniquename <- content$UniqueName
    tm1_element_object$type <- content$Type
    tm1_element_object$level <- content$Level
    tm1_element_object$index <- content$Index
    tm1_element_object$components <- content$Components$Name
    tm1_element_object$attributes <- content$Attributes

    return(tm1_element_object)

  }


  #return




}
