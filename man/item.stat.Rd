\name{item.stat}
\alias{item.stat}
\title{Item Statistics}
\description{This function computes some statistics for each item.}
\usage{
item.stat(obj, columns, weights = FALSE)
}
\arguments{
  \item{obj}{An object containing the data imported by function \code{\link{read.formscanner}}, 
		the key added by function \code{\link{addkey}} and/or weights added by function \code{\link{addweights}}.}
  \item{columns}{A vector containing which columns to use. Columns can be specified by name or number.}
	\item{weights}{Logical. If TRUE weights are used to compute the score.}
}
\value{
A data frame with the following variables.
\item{item}{item label.}
\item{score}{total score for each item. If \code{weights} is FALSE \code{score} is equal to the number of corrected responses.}
\item{max}{maximum score for each item.}
\item{perc}{ratio between score and max.}
}
\author{Michela Battauz}
\seealso{
\code{\link{person.stat}}
}
\examples{
data(test)
data(key)
data(weights)
data(weights_multiple)

testk <- addkey(test, keydata = key)
testw <- addweights(testk, weightsdata = weights)
testwm <- addweights(test, weightsdata = weights_multiple)

# number of correct responses for each item
ist <- item.stat(obj = testk, col = 2:41)
head(ist)
# sum of weights of correct responses for each item
ist <- item.stat(obj = testw, col = 2:41, weights = TRUE)
head(ist)
# sum of weights of every response for each item
ist <- item.stat(obj = testwm, col = 2:41, weights = TRUE)
head(ist)
}