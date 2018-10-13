\name{tm1_insert_dimension_element}
\alias{tm1_insert_dimension_element}
\title{TM1 Insert New Element to a Dimension}
\usage{
tm1_insert_dimension_element(tm1_connection,
  dimension, element, parent)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{dimension}{Name of dimension}
\item{element}{Name of new element}
\item{parent}{Name of parent of new element. Leave bland if there is no parent.}

}
\description{
Inserts a new element to the dimension
}
\examples{
\dontrun{
tm1_insert_dimension_element(tm1_connection("localhost", "8881", "admin", "apple"),
"month", "test", "Year")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_insert_dimension_element(con_obj, "month", "test", "Year")
}
}
