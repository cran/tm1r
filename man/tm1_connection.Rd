\name{tm1_connection}
\alias{tm1_connection}
\title{TM1 Connection}
\usage{
tm1_connection(adminhost = "", httpport = "",
username = "", password = "",
namespace="", ssl=FALSE)
}
\arguments{

\item{adminhost}{adminhost of tm1 model}
\item{httpport}{httpport of tm1 model}
\item{username}{username to connect to tm1 model}
\item{password}{password of the username}
\item{namespace}{ID of namespace should be specified if there is CAM security. Should be blank for native security}
\item{ssl}{TRUE or FALSE}

}
\description{
Creates and returns a connection object to connect to TM1 via REST API.
}
\examples{
tm1_connection("cubewise.sonorado.com", "8881", "admin", "apple")
}
