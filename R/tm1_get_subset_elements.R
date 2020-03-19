tm1_get_subset_elements <- function(tm1_connection, dimension, subset) {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  dimension <- gsub(" ", "%20", dimension, fixed=TRUE)
  subset_new <- gsub(" ", "%20", subset, fixed=TRUE)

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/"
  u6 <- "api/v1/Dimensions('"
  u7 <- dimension
  u8 <- "')/Hierarchies('"
  u9 <- dimension
  u10 <- "')/Subsets('"
  u11 <- subset_new
  u12 <- "')/Elements?$select=Name"

  # url development
  url <- ifelse(tm1_base_url=="", paste0(u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12), paste0(tm1_base_url, u6, u7, u8, u9, u10, u11, u12))
  #url = "Dimensions('DMeasure')/Hierarchies('DMeasure')/Subsets('Accounts to loop')/Elements?$select=Name"

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
  tm1_sub_els <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$value$Name

  #change to data frame
  #tm1_sub_els <- as.data.frame(tm1_sub_els)
  #colnames(tm1_sub_els)[1] <- subset
  #return
  return(tm1_sub_els)



}
