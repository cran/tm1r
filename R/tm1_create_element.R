tm1_create_element <- function(tm1_connection,
                            dimension,
                            element,
                            parent = "",
                            weight = 1,
                            type = "Numeric") {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)

  ##########################################3
  ### if the element does not exist, add it to the dimension

  if (tm1_get_element(tm1_connection, dimension, element)$index == 0) {

  u6 <- "api/v1/Dimensions('"
  u7 <- dimension
  u8 <- "')/Hierarchies('"
  u9 <- dimension
  u10 <- "')/Elements"


  # url development

  url <- paste0(tm1_base_url, u6, u7, u8, u9, u10)
  #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Elements"

  elementbody1 <- "{ \"Name\": \""
  elementbody2 <- element
  elementbody3 <-"\", "
  elementbody4 <-" \"Type\": \""
  elementbody5 <- type
  elementbody6 <-"\" }"

  elementbody <- paste0(elementbody1, elementbody2, elementbody3, elementbody4, elementbody5, elementbody6)

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

  ##########################################
  ### now the element exist. connect it to the parent if it is not a component

  elementproper <- tm1_get_element(tm1_connection, dimension, element)$name

  if (parent != "") {

    components <- tm1_get_element(tm1_connection, dimension, parent)$components
    componentcheckresult <- subset(components, components == elementproper)

  if (length(componentcheckresult) == 0) {

    u6 <- "api/v1/Dimensions('"
    u7 <- dimension
    u8 <- "')/Hierarchies('"
    u9 <- dimension
    u10 <- "')/Edges"

    # url development
    url <- paste0(tm1_base_url, u6, u7, u8, u9, u10)
    #url = "https://localhost:8881/api/v1/Dimensions('month')/Hierarchies('month')/Edges"

    elementbody1 <- "[ {    \"ParentName\": \" "
    elementbody2 <- parent
    elementbody3 <-" \", \"ComponentName\": \" "
    elementbody4 <- element
    elementbody5 <-" \", \"Weight\": "
    elementbody6 <- weight
    elementbody7 <- " } ]"

    elementbody <- paste0(elementbody1, elementbody2, elementbody3, elementbody4,
                          elementbody5, elementbody6, elementbody7)

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
  }




}
