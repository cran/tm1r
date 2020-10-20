tm1_run_process <- function(tm1_connection,
                            process = "",
                            par1name="", par1value="", par2name="", par2value="", par3name="", par3value="") {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # added because some http does not know space
  process <- gsub(" ", "%20", process, fixed=TRUE)

  u6 <- "api/v1/Processes('"
  u7 <- process
  u8 <- "')/tm1.Execute"

  # url development
  url <- paste0(tm1_base_url, u6, u7, u8)
  #url = "https://localhost:8881/api/v1/Processes('create_Y2Ksales_cube')/tm1.Execute"

  # sample parameter syntax
  parameters <-
  "{
    \"Parameters\":
    [
     {\"Name\":\"par1name\", \"Value\":\"par1value\"}
    ,{\"Name\":\"par2name\", \"Value\":par2value}
    ,{\"Name\":\"par3name\", \"Value\":par3value}
    ]
  }"

  # parameter string development
  p1 <- "{    \"Parameters\":  ["

  if(par1name != "")
  {
    p2 <- paste0("{\"Name\":\"", par1name, "\", \"Value\":\"", par1value, "\"}")
  }
  else  {    p2 <- ""  }

  if(par2name != "")
  {
    p3 <- paste0(",{\"Name\":\"", par2name, "\", \"Value\":\"", par2value, "\"}")
  }
  else  {    p3 <- ""  }

  if(par3name != "")
  {
    p4 <- paste0(",{\"Name\":\"", par3name, "\", \"Value\":\"", par3value, "\"}")
  }
  else  {    p4 <- ""  }

  p5 <- "]}"



  parameters <- paste0(p1, p2, p3, p4, p5)

  # post request
  tm1_process_return <-
    httr::POST(url,
         httr::add_headers("Authorization" = tm1_auth_key),
         httr::add_headers("Content-Type" = "application/json"),
         body = parameters)

  # return manipulation
  # if content is empty; then success
  # else get the error message to differentiate abortion and minor error
  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
  {
    tm1_process_message <- "ProcessCompletedSuccessfully"
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
