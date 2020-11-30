tm1_connection <- function(adminhost = "localhost", httpport = "",
                           username = "admin", password = "apple", namespace="", ssl=TRUE,
                           base_url = "") {

  if (base_url == "") {
    u1 <- ifelse(ssl==TRUE, "https://", "http://")
    u2 <- adminhost
    u3 <- ":"
    u4 <- httpport
    u5 <- "/"

    tm1_base_url <- paste0(u1, u2, u3, u4, u5)
  }
  else
  {
    tm1_base_url <- gsub(" ", "%20", base_url, fixed=TRUE)
  }


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

  url <- paste0(tm1_base_url, "api/v1/Configuration")
  tm1_process_return <-
    httr::GET(url,
              httr::add_headers("Authorization" = tm1_auth_key))
  TM1SessionID <- tm1_process_return$cookies$value

  tm1_connect_object <- list(base_url=c(tm1_base_url), key=c(tm1_auth_key), sessionid=c(TM1SessionID))

  return(tm1_connect_object)

}
