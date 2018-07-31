\name{tm1_run_chore}
\alias{tm1_run_chore}
\title{TM1 Run a Chore}
\usage{
tm1_run_chore(tm1_connection, chore)
}
\arguments{

\item{tm1_connection}{tm1 connection object returned by the function tm1_connection}
\item{chore}{Name of a chore as a string}

}
\description{
Runs a chore
}
\examples{
tm1_run_chore(tm1_connection("cubewise.sonorado.com", "8881", "admin", "apple"), "test")

con_obj <- tm1_connection("cubewise.sonorado.com", "8881", "admin", "apple")
tm1_run_chore(con_obj, "test")
}
