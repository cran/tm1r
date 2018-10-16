tm1_get_data <- function(tm1_connection, cube,
                                element1="", element2="", element3="", element4="", element5="",
                                element6="", element7="", element8="", element9="", element10="") {


  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/ExecuteMDX?$expand=Cells($select=Value)"

  # url development
  url <- paste0(u1, u2, u3, u4, u5)
  #url = "https://localhost:8881/api/v1/ExecuteMDX?$expand=Cells($select=Value)"

  # get dimensions of cube
  dimensions <- tm1_get_cube_dimensions(tm1_connection, cube)
  dimnumber <- length(dimensions)

  elements <- c(element1, element2, element3, element4, element5, element6, element7, element8, element9, element10)

  mapping_vector <- character(10)

  mapping_vector[1] <- paste0("[", dimensions[1],"].[", elements[1],"]")
  mapping_vector[2] <- paste0("[", dimensions[2],"].[", elements[2],"]")

  fromloop <- 3
  toloop <- dimnumber - 1
  if (dimnumber > 3)
    for (i in fromloop : toloop)
      {
        mapping_vector[i] <- paste0("[", dimensions[i],"].[", elements[i],"]", ",")
      }

  if (dimnumber > 2)
    mapping_vector[dimnumber] <- paste0("[", dimensions[dimnumber],"].[", elements[dimnumber],"]")

  # sample body syntax
  if (dimnumber > 2)
  {

    conditiontext <- paste0(
      "WHERE (",
      mapping_vector[3],
      mapping_vector[4],
      mapping_vector[5],
      mapping_vector[6],
      mapping_vector[7],
      mapping_vector[8],
      mapping_vector[9],
      mapping_vector[10],
      ")"
    )
  }
  else
    conditiontext <- ""


  bodytext <- paste0(
    " {	\"MDX\": \"SELECT {",
    mapping_vector[1],
    "} ON COLUMNS, {",
    mapping_vector[2],
    "} ON ROWS FROM [",
    cube,
    "]  ",
    conditiontext,
    "\"} ")

  # post request
  tm1_process_return <-
    httr::POST(url,
               httr::add_headers("Authorization" = tm1_auth_key),
               httr::add_headers("Content-Type" = "application/json"),
         body = bodytext)

  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
    stop()
  }

  # return manipulation
  tm1_process_message <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$Cells$Value

  return(tm1_process_message)


}
