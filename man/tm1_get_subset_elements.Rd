\name{tm1_get_subset_elements}
\alias{tm1_get_subset_elements}
\title{TM1 Get Elements of a subset}
\usage{
tm1_get_subset_elements(tm1_connection, dimension, subset)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{dimension}{Name of a dimension as a string}
\item{subset}{Name of a subset as a string}

}
\description{
Gets elements of a subset
}
\examples{
\dontrun{
tm1_get_subset_elements(
tm1_connection("localhost", "8881", "admin", "apple"),
"region", "default")

con_obj <- tm1_connection("localhost", "8881", "admin", "apple")
tm1_get_dimension_elements(con_obj, "region", "default")
}
}
