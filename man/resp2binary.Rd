\name{resp2binary}
\alias{resp2binary}
\title{Convert Responses to Binary Data}
\description{
Converts data to binary (correct/incorrect) responses, according to the key.
}
\usage{
resp2binary(obj, columns)
}
\arguments{
  \item{obj}{An object of class \code{fsdata} containing the data imported by function \code{\link{read.formscanner}}.}
  \item{columns}{A vector containing which columns to use. Columns can be specified by name or number.}
}
\value{
The data frame \code{data} contained in \code{obj} with \code{columns} replaced by binary data.
}
\author{Michela Battauz}
\seealso{
\code{\link{read.formscanner}}
}
\examples{
data(exam1)
exam1bin <- resp2binary(obj = exam1, col = 2:31)
head(exam1bin)
exam1bin <- resp2binary(obj = exam1, col = c("I1", "I2"))
head(exam1bin)
}
