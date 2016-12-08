\name{resp2scores}
\alias{resp2scores}
\title{Convert Responses to Scores}
\description{
Assigns a weight to the responses.
}
\usage{
resp2scores(obj, columns)
}
\arguments{
  \item{obj}{An object containing the data imported by function \code{\link{read.formscanner}} 
		and weights added by function \code{\link{addweights}}.}
  \item{columns}{A vector containing which columns to use. Columns can be specified by name or number.}
}
\value{
The data frame \code{data} contained in \code{obj} with \code{columns} replaced by scored responses.
}
\author{Michela Battauz}
\seealso{
\code{\link{read.formscanner}}
}
\examples{
data(test)
data(key)
data(weights)
data(weights_multiple)

testk <- addkey(test, keydata = key)
testw <- addweights(testk, weightsdata = weights)
testwm <- addweights(test, weightsdata = weights_multiple)

# ASSIGN WEIGHTS TO RESPONSES
resps <- resp2scores(obj = testw, col =2:41)
resps[, 2:5]

# ASSIGN WEIGHTS TO RESPONSES (MULTIPLE WEIGHTS)
resps <- resp2scores(obj = testwm, col =2:41)
resps[, 2:5]
}


