tm1_get_dimension_element_next <- function(tm1_connection, dimension, element, pos = 1, filter = "Numeric") {


  dimlist <- tm1_get_dimension_elements(tm1_connection, dimension)

  if (filter == "")
    dimlistfiltered <- dimlist
  else
    #dimlistfiltered <- dimlist
    dimlistfiltered <- subset(dimlist, dimlist$Type == filter)

  row.names(dimlistfiltered) <- dimlistfiltered[,1]

  elindex <- which(rownames(dimlistfiltered) == element)

  if (length(elindex) == 0) {
    message("element is not found in dimension")
    stop()
  }
  else
  {
    newelindex <- elindex + pos

    if (newelindex < 1 || newelindex > length(row.names(dimlistfiltered))) {
      message("undefined target element. check pos or filter parameters")
      stop()
    }
    else
      {
        newel <- rownames(dimlistfiltered[newelindex,])

        #return
        return(newel)
      }

  }


}
