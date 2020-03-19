\name{tm1_connection}
\alias{tm1_connection}
\title{TM1 Connection}
\usage{
tm1_connection(adminhost = "localhost", httpport = "",
username = "admin", password = "apple",
namespace="", ssl=TRUE, base_url="")
}
\arguments{

\item{adminhost}{adminhost of tm1 model}
\item{httpport}{httpport of tm1 model}
\item{username}{username to connect to tm1 model}
\item{password}{password of the username}
\item{namespace}{ID of namespace should be specified if there is CAM security. Should be blank for native security}
\item{ssl}{If UseSSL parameter is T in tm1s.cfg file, then TRUE. Else FALSE. Default is TRUE}
\item{base_url}{when connecting to cloud, this option can be used instead of adminhost and httpport}

}
\description{
Creates and returns a connection object to connect to TM1 via REST API.
}
\examples{
\dontrun{
tm1_connection("localhost", "8881", "admin", "apple")
tm1_connection(username="admin", password="apple",
base_url = "https://[Customer_Name].planning-analytics.ibmcloud.com/tm1/api/[Server Name]/")
}
}
