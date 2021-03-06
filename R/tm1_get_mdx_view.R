tm1_get_mdx_view <- function(tm1_connection, mdx="", RowElementAsColumn = FALSE) {

  tm1_auth_key <- tm1_connection$key
  tm1_base_url <- tm1_connection$base_url

  # url development
  u6 <- "api/v1/ExecuteMDX"
  u7 <- "?$expand=Axes($expand=Tuples($expand=Members($select=Name,UniqueName))),Cells($select=Value)"


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

  tm1_return <- jsonlite::fromJSON(httr::content(tm1_process_return, "text"))

  # get number of dimension elements of view
  ncols <- tm1_return$Axes$Cardinality[1]
  nrows <- ifelse(grepl("ON ROWS", mdx) == TRUE, tm1_return$Axes$Cardinality[2], 1)

  # get data from return
  tm1_data <- tm1_return$Cells$Value

  # fill NAs with zero data
  tm1_data[is.na(tm1_data)] <- 0

  # put data into matrix and convert to data frame in order to change colnames
  tm1_view_matrix <- matrix(tm1_data, nrow=nrows,ncol = ncols, byrow = TRUE)
  tm1_view_matrix <- as.data.frame(tm1_view_matrix)

  # if there is no column elements, do not run assign
  if(is.null(tm1_return$Axes$Tuples[[1]]$Members[[1]]$Name) == FALSE){
    for (i in 1:ncols)
    {
      colname <- tm1_return$Axes$Tuples[[1]]$Members[[i]]$Name
      colnames(tm1_view_matrix)[i] <- paste(colname, collapse = "|")
    }
  } else {
    colnames(tm1_view_matrix)[1] <- "Value"
  }


  # if there is no rown elements, do not run assign
  if(is.null(tm1_return$Axes$Tuples[[2]]$Members[[1]]$Name) == FALSE && grepl("ON ROWS", mdx) == TRUE){
    for (j in 1:nrows)
    {
      rowname <- tm1_return$Axes$Tuples[[2]]$Members[[j]]$Name
      rownames(tm1_view_matrix)[j] <- paste(rowname, collapse = "|")
    }
  } else {
    rownames(tm1_view_matrix)[1] <- "Value"
  }

  # add row elements as column at the end
  tm1_view_rowelements <- cbind(tm1_view_matrix, rownames(tm1_view_matrix))

  # get a data frame from row elements
  tm1_rowelements <- strsplit(as.character(tm1_view_rowelements$"rownames(tm1_view_matrix)"),'|', fixed = TRUE)

  # make it proper and data frame to assign header
  tm1_rowelements_seperate <- do.call(rbind, tm1_rowelements)
  tm1_rowelements_seperate <- as.data.frame(tm1_rowelements_seperate)

  # assign colnames to dim elements data frame
  if(nrows > 1)
  for(i in 1:ncol(tm1_rowelements_seperate))
  {
    #colnames(tm1_rowelements_seperate)[i] <- paste("Dim",as.character(i),sep="")
    dimfullname <- tm1_return$Axes$Tuples[[2]]$Members[[1]]$UniqueName[i]
    colnames(tm1_rowelements_seperate)[i] <- substr(dimfullname, 2, regexpr("]", dimfullname, fixed = TRUE)[1]-1)
  }

  # combine data and elements data frames
  tm1_view_dataframe <- cbind(tm1_view_matrix, tm1_rowelements_seperate)
  rownames(tm1_view_dataframe) <- NULL


  # if parameter is FALSE, return data only matrix
  if( RowElementAsColumn == FALSE || nrows == 1)
    return(tm1_view_matrix)
  # if parameter is TRUE, return data.frame including data and elements
  else
    return(tm1_view_dataframe)


}
