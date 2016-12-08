\name{person.stat}
\alias{person.stat}
\title{Person Statistics}
\description{This function computes some statistics for each person.}
\usage{
person.stat(obj, columns, weights = FALSE)
}
\arguments{
  \item{obj}{An object containing the data imported by function \code{\link{read.formscanner}}, 
		the key added by function \code{\link{addkey}} and/or weights added by function \code{\link{addweights}}.}
  \item{columns}{A vector containing which columns to use. Columns can be specified by name or number.}
	\item{weights}{Logical. If TRUE weights are used to compute the score.}
}
\value{
A data frame with the following variables.
\item{id}{person label.}
\item{score}{total score for each person. If \code{weights} is FALSE \code{score} is equal to the number of corrected responses.}
\item{max}{maximum score for each person.}
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

# number of correct responses for each person (only 4 items)
pst <- person.stat(obj = testk, col = 2:5)
pst
# sum of weights of correct responses for each person
pst <- person.stat(obj = testw, col = 2:5, weights = TRUE)
pst
# sum of weights of every response for each person
pst <- person.stat(obj = testwm, col = 2:5, weights = TRUE)
pst
}
