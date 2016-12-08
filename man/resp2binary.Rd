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
  \item{obj}{An object containing the data imported by function \code{\link{read.formscanner}} 
		and the key added by function \code{\link{addkey}}.}
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
data(test)
data(key)

testk <- addkey(test, keydata = key)

resp01 <- resp2binary(obj = testk, col = 2:41)
resp01
}
