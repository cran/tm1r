\name{tm1_get_dimension_element_next}
\alias{tm1_get_dimension_element_next}
\title{TM1 Get the next element in a dimension}
\usage{
tm1_get_dimension_element_next(tm1_connection, dimension, element, pos, filter)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{dimension}{Name of a dimension as a string}
\item{element}{Name of a element as a string}
\item{pos}{Number of position lag. ie: pos=1 means next element and pos=-1 means previous element. Default is 1.}
\item{filter}{filter on element type. Must be one of following: Numeric, Consolidated, String, or empty}
}
\description{
Gets next element in a dimension
}
\examples{
tm1_get_dimension_element_next(
  tm1_connection("cubewise.sonorado.com", "8881", "admin", "apple"),
  "month", "Jun")

con_obj <- tm1_connection("cubewise.sonorado.com", "8881", "admin", "apple")
tm1_get_dimension_element_next(con_obj, "month", "Jun", pos = 2, filter="Numeric")
}
