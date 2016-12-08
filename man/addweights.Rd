\name{addweights}
\alias{addweights}
\title{Add a Weights to a Data Frame}
\description{
Adds the weights associated with the responses to the items to a data frame.
}
\usage{
addweights(obj, weightsfile = NULL, weightsdata = NULL)
}
\arguments{
  \item{obj}{An object containing the data imported by function \code{\link{read.formscanner}}.}
	\item{weightsfile}{the name of the file with the weights. 
		Column names should match the names of the items.}
	\item{weightsdata}{an R data frame containing the weights. 
		Column names should match the names of the items.}
}
\details{
If \code{weightsfile} and \code{weightsdata} have only one row, they should contain the weights that
are assigned to the correct responses. These are defined by \code{\link{addkey}}.
To specify a different weight to every response of each item, \code{weightsfile} and \code{weightsdata} 
should have one row for each response. In this case there should be a field named \code{response}.

\code{weightsfile} and \code{weightsdata} can contain more items than \code{obj}.
In this case, items not contained in \code{obj} are dropped in the output.
}
\value{
A list with \code{data}, eventually \code{key}, and \code{weights}.
}
\author{Michela Battauz}
\seealso{
\code{\link{addkey}}
}
\examples{
data(test)
data(key)
data(weights)
data(weights_multiple)
testk <- addkey(test, keydata = key)
testw <- addweights(testk, weightsdata = weights)
testwm <- addweights(test, weightsdata = weights_multiple)
}

