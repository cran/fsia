\name{item.stat}
\alias{item.stat}
\title{Item Statistics}
\description{This function computes some statistics for each item.}
\usage{
item.stat(obj, columns)
}
\arguments{
  \item{obj}{An object of class \code{fsdata} containing the data imported by function \code{\link{read.formscanner}}.}
  \item{columns}{A vector containing which columns to use. Columns can be specified by name or number.}
}
\value{
A data frame with the following variables.
\item{item}{item label.}
\item{score}{number of corrected responses.}
\item{count}{number of total responses.}
\item{perc}{percentage of corrected responses.}
}
\author{Michela Battauz}
\seealso{
\code{\link{person.stat}}
}
\examples{
data(exam1)
item.stat(obj = exam1, col = 2:31)
item.stat(obj = exam1, col = c("I1","I2"))
}