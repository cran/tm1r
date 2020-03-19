tm1_connection <- function(adminhost = "localhost", httpport = "",
                           username = "admin", password = "apple", namespace="", ssl=TRUE,
                           base_url = "") {

  tm1_adminhost <- adminhost
  tm1_httpport <- httpport
  tm1_base_url <- gsub(" ", "%20", base_url, fixed=TRUE)

  if( namespace == "")
  {
    usernamepwd <- paste(username, password, sep=":")
    auth_type <- "Basic"
  }
  else
  {
    usernamepwd <- paste(username, password, namespace, sep=":")
    auth_type <- "CAMNamespace"
  }

  tm1_auth_key <- paste(auth_type, jsonlite::base64_enc(usernamepwd), sep=" ")

  httr::set_config(httr::config(ssl_verifypeer = FALSE))
  httr::set_config(httr::config(ssl_verifyhost = FALSE))


  tm1_connect_object <- list(adminhost=c(tm1_adminhost), port=c(tm1_httpport), key=c(tm1_auth_key), ssl=c(ssl), base_url=c(tm1_base_url))

  tm1_get_config(tm1_connect_object)

  return(tm1_connect_object)

}
