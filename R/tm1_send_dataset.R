tm1_send_dataset <- function(tm1_connection,
                          valueset, cube, rowdim, coldim,
                          rowdim2 = "", rowdim3 = "", rowdim4 = "", rowdim5 = "",
                          titledim1 = "", titleel1 = "", titledim2 = "", titleel2 = "",
                          titledim3 = "", titleel3 = "", titledim4 = "", titleel4 = "",
                          titledim5 = "", titleel5 = "", titledim6 = "", titleel6 = "",
                          titledim7 = "", titleel7 = "", titledim8 = "", titleel8 = ""
                          ) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  ###########################################
  ###########mdx generate

  mdxselect <- "Select {"

  ### row mdx build
  mdxrown <- nrow(valueset)
  mdxrowlist <- strsplit(rownames(valueset), "|", TRUE)
  mdxrowlist2 <- character(mdxrown)
  for (i in 1:mdxrown) {
    mdxrowlist[[i]][1] <- ifelse(rowdim != "", paste0( "[", rowdim, "].[", mdxrowlist[[i]][1], "]"), "")
    mdxrowlist[[i]][2] <- ifelse(rowdim2 != "", paste0( "[", rowdim2, "].[", mdxrowlist[[i]][2], "]"), "")
    mdxrowlist[[i]][3] <- ifelse(rowdim3 != "", paste0( "[", rowdim3, "].[", mdxrowlist[[i]][3], "]"), "")
    mdxrowlist[[i]][4] <- ifelse(rowdim4 != "", paste0( "[", rowdim4, "].[", mdxrowlist[[i]][4], "]"), "")
    mdxrowlist[[i]][5] <- ifelse(rowdim5 != "", paste0( "[", rowdim5, "].[", mdxrowlist[[i]][5], "]"), "")

    mdxrowlisttemp <- mdxrowlist[[i]]

    mdxrowlist2[i] <- paste0( "{", paste(mdxrowlisttemp[mdxrowlisttemp != ""], collapse = "}*{"), "}")
  }

  mdxrows <- paste(mdxrowlist2, collapse = ",")

  mdxrowend <- "} ON ROWS,{"

  ### column mdx build
  mdxcols <- paste0("[", coldim, "].[",paste(colnames(valueset), collapse = paste0("],[", coldim, "].[")),"]")

  mdxcolend <- "} ON COLUMNS "

  mdxcube <- paste0("FROM [", cube, "]")

  mdxwherelist <- character(0)
  mdxwherelist[1] <- ifelse(titledim1 != "", paste0("[", titledim1, "].[", titleel1, "]"), "")
  mdxwherelist[2] <- ifelse(titledim2 != "", paste0("[", titledim2, "].[", titleel2, "]"), "")
  mdxwherelist[3] <- ifelse(titledim3 != "", paste0("[", titledim3, "].[", titleel3, "]"), "")
  mdxwherelist[4] <- ifelse(titledim4 != "", paste0("[", titledim4, "].[", titleel4, "]"), "")
  mdxwherelist[5] <- ifelse(titledim5 != "", paste0("[", titledim5, "].[", titleel5, "]"), "")
  mdxwherelist[6] <- ifelse(titledim6 != "", paste0("[", titledim6, "].[", titleel6, "]"), "")
  mdxwherelist[7] <- ifelse(titledim7 != "", paste0("[", titledim7, "].[", titleel7, "]"), "")
  mdxwherelist[8] <- ifelse(titledim8 != "", paste0("[", titledim8, "].[", titleel8, "]"), "")

  #mdxwhere <- paste0(" WHERE (", paste(mdxwherelist[mdxwherelist != ""], collapse = ','), ")")
  mdxwhere <- ifelse( paste(mdxwherelist[mdxwherelist != ""], collapse = ',') == "", "", paste0(" WHERE (", paste(mdxwherelist[mdxwherelist != ""], collapse = ','), ")"))


  mdx <- paste0(mdxselect, mdxrows, mdxrowend, mdxcols, mdxcolend, mdxcube, mdxwhere)

  ###########################################
  ########### construct mdx to get id

  # url development
  u6 <- "api/v1/ExecuteMDX"
  #u6 <- "?$expand=Axes($expand=Tuples($expand=Members($select=Name,UniqueName))),Cells($select=Value)"
  u7 <- ""

  url <- paste0(tm1_base_url, u6, u7)
  #url = "https://localhost:8881/api/v1/ExecuteMDX?
  #$expand=Axes($expand=Tuples($expand=Members($select=Name))),Cells($select=Value)"

  # change mdx to body text
  bodytext <- paste0("{	\"MDX\": \" ", mdx, "\"}")

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

  # Get the CellSetID
  tm1_return <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))
  CellSetID <- tm1_return$ID

  ###########################################
  ########### SEnd the values
  # url development
  u6 <- "api/v1/Cellsets('"
  u7 <- CellSetID
  u8 <- "')/Cells"

  url <- paste0(tm1_base_url, u6, u7, u8)

  # change body to values
  nrown <- nrow(valueset)
  ncoln <- ncol(valueset)
  cellnumber <- nrown * ncoln
  ordinalvalues <- character(cellnumber)
  ordinalnumber <- 0

  for (i in 1:nrown) {

    for (j in 1:ncoln) {

      ordinalvalues[ordinalnumber+1] <- paste0("{ \"Ordinal\": ", ordinalnumber, ", \"Value\": \"", valueset[i,j], "\" }")
      ordinalnumber <- ordinalnumber + 1
    }

  }

  bodytext <- ""
  bodytext <- paste0(bodytext, "[")
  ordinalvaluesstr <- paste(ordinalvalues, collapse = ',')
  bodytext <- paste0(bodytext, ordinalvaluesstr)
  bodytext <- paste0(bodytext, "]")

  # patch request
  tm1_process_return <-
    httr::PATCH(url,
               httr::add_headers("Authorization" = tm1_auth_key),
               httr::add_headers("Content-Type" = "application/json"),
               body = bodytext)

  # check return if error
  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
  {
    # Do nothing.
  }
  else
  {
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
    }
  }

  ###########################################
  ###########Delete the CellSet
  # url development
  u6 <- "api/v1/Cellsets('"
  u7 <- CellSetID
  u8 <- "')"

  url <- paste0(tm1_base_url, u6, u7, u8)

  #Delete request
  tm1_process_return <-
    httr::DELETE(url,
               httr::add_headers("Authorization" = tm1_auth_key),
               httr::add_headers("Content-Type" = "application/json"))

  # check return if error
  if(httr::content(tm1_process_return, "text", encoding = "UTF-8") == "")
  {
    # Do nothing.
  }
  else
  {
    if (is.null(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message) == FALSE) {
      message(jsonlite::fromJSON(httr::content(tm1_process_return, "text"))$error$message)
      stop()
    }
  }

}
