tm1_send_data <- function(tm1_connection,
                          value, cube, element1="", element2="", element3="", element4="", element5="",
                                element6="", element7="", element8="", element9="", element10="", increment=FALSE) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key

  # if increment, change value
  if( increment == FALSE )
  {
    value <- value
  }
  else
  {
    currvalue <- tm1_get_data(tm1_connection, cube, element1, element2, element3, element4, element5,
                              element6, element7, element8, element9, element10)
    if (class(value) == "numeric" && class(currvalue) == "numeric" )
      value <- value + currvalue

  }

  # added because some http does not know space
  cube <- gsub(" ", "%20", cube, fixed=TRUE)

  u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/Cubes('"
  u6 <- cube
  u7 <- "')/tm1.Update"

  # url development
  url <- paste(u1, u2, u3, u4, u5, u6, u7, sep = "")
  #url = "https://localhost:8881/api/v1/Cubes('{{cubeName}}')/tm1.Update"

  # get dimensions of cube
  dimensions <- tm1_get_cube_dimensions(tm1_connection, cube)
  dimnumber <- length(dimensions)

  elements <- c(element1, element2, element3, element4, element5, element6, element7, element8, element9, element10)

  mapping_vector <- character(10)

  for (i in 1 : dimnumber-1)
  {
    mapping_vector[i] <- paste(
                               "\"Dimensions('",
                               dimensions[i],
                               "')/Hierarchies('",
                               dimensions[i],
                               "')/Elements('",
                               elements[i],
                               "')\",",
                               sep="")
  }
  mapping_vector[dimnumber] <- paste(
    "\"Dimensions('",
    dimensions[dimnumber],
    "')/Hierarchies('",
    dimensions[dimnumber],
    "')/Elements('",
    elements[dimnumber],
    "')\"",
    sep="")

  # sample body syntax
  bodytext <- paste(
    "{
	\"Cells\":[
  {\"Tuple@odata.bind\": [",
    mapping_vector[1],
    mapping_vector[2],
    mapping_vector[3],
    mapping_vector[4],
    mapping_vector[5],
    mapping_vector[6],
    mapping_vector[7],
    mapping_vector[8],
    mapping_vector[9],
    mapping_vector[10],

  "  ]}],  \"Value\":\"", value, "\"  }", sep="")

  # post request
  tm1_process_return <-
    httr::POST(url,
         httr::add_headers("Authorization" = tm1_auth_key),
         httr::add_headers("Content-Type" = "application/json"),
         body = bodytext)

  # return manipulation
  # if content is empty; then success
  # else get the error message to differentiate abortion and minor error
  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
  {
    tm1_process_message <- "DataSentSuccessfully"
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