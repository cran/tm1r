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
    rowdim1str <- paste0("{[",rowdim1, "].[",elvec[1], "]")
    i <- 2
    while(i <= ellen)
      {
      rowdim1str <- paste0(rowdim1str, ", [", rowdim1, "].[",elvec[i], "]")
      i <- i + 1
      }
    rowdim1str <- paste0(rowdim1str, "}")
    }
  else
    rowdim1str <- paste0("{TM1SubsetToSet([",rowdim1, "].[",rowdim1, "], \"",rowsub1, "\")}")

  # ROW DIM 2 STR GENERATION
  if (rowdim2 == "")
    rowdim2str <- ""
  else
    if (rowsub2 == "")
    {
      elvec <- strsplit(rowel2, "|", TRUE)[[1]]
      ellen <- length(elvec)
      rowdim2str <- paste0("{[",rowdim2, "].[",elvec[1], "]")
      i <- 2
      while(i <= ellen)
      {
        rowdim2str <- paste0(rowdim2str, ", [", rowdim2, "].[",elvec[i], "]")
        i <- i + 1
      }
      rowdim2str <- paste0(rowdim2str, "}")
    }
  else
    rowdim2str <- paste0("{TM1SubsetToSet([",rowdim2, "].[",rowdim2, "], \"",rowsub2, "\")}")


  # ROW DIM 3 STR GENERATION
  if (rowdim3 == "")
    rowdim3str <- ""
  else
    if (rowsub3 == "")
    {
      elvec <- strsplit(rowel3, "|", TRUE)[[1]]
      ellen <- length(elvec)
      rowdim3str <- paste0("{[",rowdim3, "].[",elvec[1], "]")
      i <- 2
      while(i <= ellen)
      {
        rowdim3str <- paste0(rowdim3str, ", [", rowdim3, "].[",elvec[i], "]")
        i <- i + 1
      }
      rowdim3str <- paste0(rowdim3str, "}")
    }
  else
    rowdim3str <- paste0("{TM1SubsetToSet([",rowdim3, "].[",rowdim3, "], \"",rowsub3, "\")}")



  # col DIM 1 STR GENERATION
  if (coldim1 == "")
    coldim1str <- ""
  else
    if (colsub1 == "")
    {
      elvec <- strsplit(colel1, "|", TRUE)[[1]]
      ellen <- length(elvec)
      coldim1str <- paste0("{[",coldim1, "].[",elvec[1], "]")
      i <- 2
      while(i <= ellen)
      {
        coldim1str <- paste0(coldim1str, ", [", coldim1, "].[",elvec[i], "]")
        i <- i + 1
      }
      coldim1str <- paste0(coldim1str, "}")
    }
  else
    coldim1str <- paste0("{TM1SubsetToSet([",coldim1, "].[",coldim1, "], \"",colsub1, "\")}")

  # col DIM 2 STR GENERATION
  if (coldim2 == "")
    coldim2str <- ""
  else
    if (colsub1 == "")
    {
      elvec <- strsplit(colel2, "|", TRUE)[[1]]
      ellen <- length(elvec)
      coldim2str <- paste0("{[",coldim2, "].[",elvec[1], "]")
      i <- 2
      while(i <= ellen)
      {
        coldim2str <- paste0(coldim2str, ", [", coldim2, "].[",elvec[i], "]")
        i <- i + 1
      }
      coldim2str <- paste0(coldim2str, "}")
    }
  else
    coldim2str <- paste0("{TM1SubsetToSet([",coldim2, "].[",coldim2, "], \"",colsub1, "\")}")

title1str <- ifelse(titledim1=="", "", paste0("[",titledim1, "].[",titledim1, "].[",titleel1, "]"))
title2str <- ifelse(titledim2=="", "", paste0("[",titledim2, "].[",titledim2, "].[",titleel2, "]"))
title3str <- ifelse(titledim3=="", "", paste0("[",titledim3, "].[",titledim3, "].[",titleel3, "]"))
title4str <- ifelse(titledim4=="", "", paste0("[",titledim4, "].[",titledim4, "].[",titleel4, "]"))
title5str <- ifelse(titledim5=="", "", paste0("[",titledim5, "].[",titledim5, "].[",titleel5, "]"))
title6str <- ifelse(titledim6=="", "", paste0("[",titledim6, "].[",titledim6, "].[",titleel6, "]"))
title7str <- ifelse(titledim7=="", "", paste0("[",titledim7, "].[",titledim7, "].[",titleel7, "]"))
title8str <- ifelse(titledim8=="", "", paste0("[",titledim8, "].[",titledim8, "].[",titleel8, "]"))
title9str <- ifelse(titledim9=="", "", paste0("[",titledim9, "].[",titledim9, "].[",titleel9, "]"))
title10str <- ifelse(titledim10=="", "", paste0("[",titledim10, "].[",titledim10, "].[",titleel10, "]"))

titleallstr <- paste0(title1str, title2str, title3str, title4str, title5str,
                     title6str, title7str, title8str, title9str, title10str)

mdx <- "SELECT "

mdx <- paste0( mdx, ifelse(colsuppress==TRUE, "NON EMPTY ", ""))
mdx <- paste0( mdx, coldim1str)
mdx <- paste0( mdx, ifelse(coldim2str=="", "", paste0("*", coldim2str)))
mdx <- paste0( mdx, " ON COLUMNS, ", sep="")

mdx <- paste0( mdx, ifelse(rowsuppress==TRUE, "NON EMPTY ", ""))
mdx <- paste0( mdx, rowdim1str)
mdx <- paste0( mdx, ifelse(rowdim2str=="", "", paste0("*", rowdim2str)))
mdx <- paste0( mdx, ifelse(rowdim3str=="", "", paste0("*", rowdim3str)))
mdx <- paste0( mdx, " ON ROWS ", sep="")

mdx <- paste0( mdx, " FROM [", cube, "] ")

if (titleallstr != "") {

  mdx <- paste0( mdx, " WHERE(", sep="")

  mdx <- paste0( mdx, title1str)
  mdx <- paste0( mdx, ifelse(title2str=="", "", paste0(", ", title2str)))
  mdx <- paste0( mdx, ifelse(title3str=="", "", paste0(", ", title3str)))
  mdx <- paste0( mdx, ifelse(title4str=="", "", paste0(", ", title4str)))
  mdx <- paste0( mdx, ifelse(title5str=="", "", paste0(", ", title5str)))
  mdx <- paste0( mdx, ifelse(title6str=="", "", paste0(", ", title6str)))
  mdx <- paste0( mdx, ifelse(title7str=="", "", paste0(", ", title7str)))
  mdx <- paste0( mdx, ifelse(title8str=="", "", paste0(", ", title8str)))
  mdx <- paste0( mdx, ifelse(title9str=="", "", paste0(", ", title9str)))
  mdx <- paste0( mdx, ifelse(title10str=="", "", paste0(", ", title10str)))

  mdx <- paste0( mdx, " )")

}


return(mdx)

}
