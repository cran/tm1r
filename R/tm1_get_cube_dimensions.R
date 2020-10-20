tm1_get_cube_dimensions <- function(tm1_connection, cube) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  cube <- gsub(" ", "%20", cube, fixed=TRUE)

  u6 <- "api/v1/Cubes('"
  u7 <- cube
  u8 <- "')/Dimensions"


  # url development
  url <- paste0(tm1_base_url, u6, u7, u8)
  #url = "https://localhost:8881/api/v1/Cubes('SalesCube')/Dimensions"

  # post request
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))

  # check return if error
  if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
    message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
    stop()
  }

  # make it proper
  tm1_cube_dims <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$value

  #change to data frame
  tm1_cube_dims <- as.data.frame(tm1_cube_dims)

  #delete 1st column
  tm1_cube_dims <- tm1_cube_dims[,c(2) ]

  #return
  return(tm1_cube_dims)



}
