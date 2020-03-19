tm1_get_data <- function(tm1_connection, cube,
                                element1="", element2="", element3="", element4="", element5="",
                                element6="", element7="", element8="", element9="", element10="") {


  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key
  tm1_ssl <- tm1_connection$ssl
  tm1_base_url <- tm1_connection$base_url

  u1 <- ifelse(tm1_ssl==TRUE, "https://", "http://")
  #u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/"
  u6 <- "api/v1/ExecuteMDX?$expand=Cells($select=Value)"

  # url development
  url <- ifelse(tm1_base_url=="", paste0(u1, u2, u3, u4, u5, u6), paste0(tm1_base_url, u6))
  #url = "https://localhost:8881/api/v1/ExecuteMDX?$expand=Cells($select=Value)"

  # get dimensions of cube
  dimlist <- tm1_get_cube_dimensions(tm1_connection, cube)
  dimnumber <- length(dimlist)

  elements <- c(element1, element2, element3, element4, element5, element6, element7, element8, element9, element10)
  dimensions <- character(10)
  dimensions <- replace(dimensions, 1:dimnumber, dimlist)

  #getmdx
  mdx <- tm1_create_mdx(cube, rowdim1 = dimensions[1], rowel1 = elements[1],
                                coldim1 = dimensions[2], colel1 = elements[2],
                                titledim1 = dimensions[3], titleel1 = elements[3],
                                titledim2 = dimensions[4], titleel2 = elements[4],
                                titledim3 = dimensions[5], titleel3 = elements[5],
                                titledim4 = dimensions[6], titleel4 = elements[6],
                                titledim5 = dimensions[7], titleel5 = elements[7],
                                titledim6 = dimensions[8], titleel6 = elements[8],
                                titledim7 = dimensions[9], titleel7 = elements[9],
                                titledim8 = dimensions[10], titleel8 = elements[10],
                                rowsuppress = FALSE, colsuppress = FALSE)

  resultview <- tm1_get_mdx_view(tm1_connection, mdx, RowElementAsColumn = FALSE)

  if (class(resultview[1,1]) == "factor") {
    return(as.character(resultview[1,1]))
  }
  else
  {
    return(resultview[1,1])
  }




}
