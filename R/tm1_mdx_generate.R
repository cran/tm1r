tm1_mdx_generate <- function(cube,
                             rowdim1="", rowsub1="", rowel1="",
                             rowdim2="", rowsub2="", rowel2="",
                             rowdim3="", rowsub3="", rowel3="",
                             coldim1="", colsub1="", colel1="",
                             coldim2="", colsub2="", colel2="",
                             titledim1="", titleel1="",
                             titledim2="", titleel2="",
                             titledim3="", titleel3="",
                             titledim4="", titleel4="",
                             titledim5="", titleel5="",
                             titledim6="", titleel6="",
                             titledim7="", titleel7="",
                             titledim8="", titleel8="",
                             titledim9="", titleel9="",
                             titledim10="", titleel10="",
                             rowsuppress=TRUE, colsuppress = TRUE
                             ) {

# ROW DIM 1 STR GENERATION
if (rowdim1 == "")
  rowdim1str <- ""
else
  if (rowsub1 == "")
    {
    elvec <- strsplit(rowel1, "|", TRUE)[[1]]
    ellen <- length(elvec)
    rowdim1str <- paste("{[",rowdim1, "].[",elvec[1], "]", sep="")
    i <- 2
    while(i <= ellen)
      {
      rowdim1str <- paste(rowdim1str, ", [", rowdim1, "].[",elvec[i], "]", sep="")
      i <- i + 1
      }
    rowdim1str <- paste(rowdim1str, "}", sep="")
    }
  else
    rowdim1str <- paste("{TM1SubsetToSet([",rowdim1, "].[",rowdim1, "], \"",rowsub1, "\")}",sep="")

  # ROW DIM 2 STR GENERATION
  if (rowdim2 == "")
    rowdim2str <- ""
  else
    if (rowsub2 == "")
    {
      elvec <- strsplit(rowel2, "|", TRUE)[[1]]
      ellen <- length(elvec)
      rowdim2str <- paste("{[",rowdim2, "].[",elvec[1], "]", sep="")
      i <- 2
      while(i <= ellen)
      {
        rowdim2str <- paste(rowdim2str, ", [", rowdim2, "].[",elvec[i], "]", sep="")
        i <- i + 1
      }
      rowdim2str <- paste(rowdim2str, "}", sep="")
    }
  else
    rowdim2str <- paste("{TM1SubsetToSet([",rowdim2, "].[",rowdim2, "], \"",rowsub2, "\")}",sep="")


  # ROW DIM 3 STR GENERATION
  if (rowdim3 == "")
    rowdim3str <- ""
  else
    if (rowsub3 == "")
    {
      elvec <- strsplit(rowel3, "|", TRUE)[[1]]
      ellen <- length(elvec)
      rowdim3str <- paste("{[",rowdim3, "].[",elvec[1], "]", sep="")
      i <- 2
      while(i <= ellen)
      {
        rowdim3str <- paste(rowdim3str, ", [", rowdim3, "].[",elvec[i], "]", sep="")
        i <- i + 1
      }
      rowdim3str <- paste(rowdim3str, "}", sep="")
    }
  else
    rowdim3str <- paste("{TM1SubsetToSet([",rowdim3, "].[",rowdim3, "], \"",rowsub3, "\")}",sep="")



  # col DIM 1 STR GENERATION
  if (coldim1 == "")
    coldim1str <- ""
  else
    if (colsub1 == "")
    {
      elvec <- strsplit(colel1, "|", TRUE)[[1]]
      ellen <- length(elvec)
      coldim1str <- paste("{[",coldim1, "].[",elvec[1], "]", sep="")
      i <- 2
      while(i <= ellen)
      {
        coldim1str <- paste(coldim1str, ", [", coldim1, "].[",elvec[i], "]", sep="")
        i <- i + 1
      }
      coldim1str <- paste(coldim1str, "}", sep="")
    }
  else
    coldim1str <- paste("{TM1SubsetToSet([",coldim1, "].[",coldim1, "], \"",colsub1, "\")}",sep="")

  # col DIM 2 STR GENERATION
  if (coldim2 == "")
    coldim2str <- ""
  else
    if (colsub1 == "")
    {
      elvec <- strsplit(colel2, "|", TRUE)[[1]]
      ellen <- length(elvec)
      coldim2str <- paste("{[",coldim2, "].[",elvec[1], "]", sep="")
      i <- 2
      while(i <= ellen)
      {
        coldim2str <- paste(coldim2str, ", [", coldim2, "].[",elvec[i], "]", sep="")
        i <- i + 1
      }
      coldim2str <- paste(coldim2str, "}", sep="")
    }
  else
    coldim2str <- paste("{TM1SubsetToSet([",coldim2, "].[",coldim2, "], \"",colsub1, "\")}",sep="")

title1str <- ifelse(titledim1=="", "", paste("[",titledim1, "].[",titledim1, "].[",titleel1, "]", sep=""))
title2str <- ifelse(titledim2=="", "", paste("[",titledim2, "].[",titledim2, "].[",titleel2, "]", sep=""))
title3str <- ifelse(titledim3=="", "", paste("[",titledim3, "].[",titledim3, "].[",titleel3, "]", sep=""))
title4str <- ifelse(titledim4=="", "", paste("[",titledim4, "].[",titledim4, "].[",titleel4, "]", sep=""))
title5str <- ifelse(titledim5=="", "", paste("[",titledim5, "].[",titledim5, "].[",titleel5, "]", sep=""))
title6str <- ifelse(titledim6=="", "", paste("[",titledim6, "].[",titledim6, "].[",titleel6, "]", sep=""))
title7str <- ifelse(titledim7=="", "", paste("[",titledim7, "].[",titledim7, "].[",titleel7, "]", sep=""))
title8str <- ifelse(titledim8=="", "", paste("[",titledim8, "].[",titledim8, "].[",titleel8, "]", sep=""))
title9str <- ifelse(titledim9=="", "", paste("[",titledim9, "].[",titledim9, "].[",titleel9, "]", sep=""))
title10str <- ifelse(titledim10=="", "", paste("[",titledim10, "].[",titledim10, "].[",titleel10, "]", sep=""))

titleallstr <- paste(title1str, title2str, title3str, title4str, title5str,
                     title6str, title7str, title8str, title9str, title10str, sep="")

mdx <- "SELECT "

mdx <- paste( mdx, ifelse(colsuppress==TRUE, "NON EMPTY ", ""), sep="")
mdx <- paste( mdx, coldim1str, sep="")
mdx <- paste( mdx, ifelse(coldim2str=="", "", paste("*", coldim2str, sep="")), sep="")
mdx <- paste( mdx, " ON COLUMNS, ", sep="")

mdx <- paste( mdx, ifelse(rowsuppress==TRUE, "NON EMPTY ", ""), sep="")
mdx <- paste( mdx, rowdim1str, sep="")
mdx <- paste( mdx, ifelse(rowdim2str=="", "", paste("*", rowdim2str, sep="")), sep="")
mdx <- paste( mdx, ifelse(rowdim3str=="", "", paste("*", rowdim3str, sep="")), sep="")
mdx <- paste( mdx, " ON ROWS ", sep="")

mdx <- paste( mdx, " FROM [", cube, "] ", sep="")

if (titleallstr != "") {

  mdx <- paste( mdx, " WHERE(", sep="")

  mdx <- paste( mdx, title1str, sep="")
  mdx <- paste( mdx, ifelse(title2str=="", "", paste(", ", title2str, sep="")), sep="")
  mdx <- paste( mdx, ifelse(title3str=="", "", paste(", ", title3str, sep="")), sep="")
  mdx <- paste( mdx, ifelse(title4str=="", "", paste(", ", title4str, sep="")), sep="")
  mdx <- paste( mdx, ifelse(title5str=="", "", paste(", ", title5str, sep="")), sep="")
  mdx <- paste( mdx, ifelse(title6str=="", "", paste(", ", title6str, sep="")), sep="")
  mdx <- paste( mdx, ifelse(title7str=="", "", paste(", ", title7str, sep="")), sep="")
  mdx <- paste( mdx, ifelse(title8str=="", "", paste(", ", title8str, sep="")), sep="")
  mdx <- paste( mdx, ifelse(title9str=="", "", paste(", ", title9str, sep="")), sep="")
  mdx <- paste( mdx, ifelse(title10str=="", "", paste(", ", title10str, sep="")), sep="")

  mdx <- paste( mdx, " )", sep="")

}

#mdx <- paste(rowdim1str,rowdim2str, rowdim3str, coldim1str, coldim2str, sep="")

return(mdx)

}
