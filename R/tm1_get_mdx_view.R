tm1_get_mdx_view <- function(tm1_connection, mdx="") {

  tm1_adminhost <- tm1_connection$adminhost
  tm1_httpport <- tm1_connection$port
  tm1_auth_key <- tm1_connection$key

  RowElementAsColumn = TRUE

  # url development
  u1 <- "https://"
  u2 <- tm1_adminhost
  u3 <- ":"
  u4 <- tm1_httpport
  u5 <- "/api/v1/ExecuteMDX"
  u6 <- "?$expand=Axes($expand=Tuples($expand=Members($select=Name,UniqueName))),Cells($select=Value)"


  url <- paste(u1, u2, u3, u4, u5, u6, sep = "")
  #url = "https://localhost:8881/api/v1/ExecuteMDX?
  #$expand=Axes($expand=Tuples($expand=Members($select=Name))),Cells($select=Value)"

  # change mdx to body text
  bodytext <- paste("{	\"MDX\": \" ", mdx, "\"}", sep="")

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
  ncols <- tm1_return$Axes[1,2]
  nrows <- tm1_return$Axes[2,2]

  # get data from return
  tm1_data <- tm1_return$Cells[2][[1]]

  # fill NAs with zero data
  tm1_data[is.na(tm1_data)] <- 0

  # put data into matrix and convert to data frame in order to change colnames
  tm1_view_matrix <- matrix(tm1_data, nrow=nrows,ncol = ncols, byrow = TRUE)
  tm1_view_matrix <- as.data.frame(tm1_view_matrix)

  # if there is no column elements, do not run assign
  if(ncols > 1 || is.null(tm1_return$Axes[1,3][[1]]$Members[[1]]$Name)==FALSE)
  {

    for (i in 1:ncols)
      {
        colname <- tm1_return$Axes[1,3][[1]]$Members[[i]]$Name
        colnames(tm1_view_matrix)[i] <- paste(colname, collapse = "|")
      }
  }

  # if there is no column elements, assign value to 1 column
  if (ncols ==1)
    colnames(tm1_view_matrix)[1] <- "Value"


  # if there is no rown elements, do not run assign
  if(nrows > 1 || is.null(tm1_return$Axes[2,3][[1]]$Members[[1]]$Name)==FALSE)
  {
    for (j in 1:nrows)
    {
      rowname <- tm1_return$Axes[2,3][[1]]$Members[[j]]$Name
      rownames(tm1_view_matrix)[j] <- paste(rowname, collapse = "|")
    }
  }

  # if there is no row elements, assign value to 1 row
  if (nrows ==1)
    rownames(tm1_view_matrix)[1] <- "Value"

  # change to matrix if there is only numeric inside matrix because matrix is better and faster
  tm1_view_matrix <- as.matrix(tm1_view_matrix)

  # add row elements as column at the end
  tm1_view_rowelements <- cbind(as.data.frame(tm1_view_matrix), rownames(tm1_view_matrix))

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
    dimfullname <- tm1_return$Axes[2,3][[1]]$Members[[1]]$UniqueName[i]
    colnames(tm1_rowelements_seperate)[i] <- substr(dimfullname, 2, regexpr("]", dimfullname, fixed = TRUE)[1]-1)
  }

  # combine data and elements data frames
  tm1_view_dataframe <- cbind(as.data.frame(tm1_view_matrix), tm1_rowelements_seperate)
  rownames(tm1_view_dataframe) <- NULL


  # if parameter is FALSE, return data only matrix
  if( RowElementAsColumn == FALSE || nrows == 1)
    return(as.data.frame(tm1_view_matrix))
  # if parameter is TRUE, return data.frame including data and elements
  else
    return(tm1_view_dataframe)


}
