\name{addkey}
\alias{addkey}
\title{Add a Key to a Data Frame}
\description{
Adds the correct responses (key) to a data frame.
}
\usage{
addkey(obj, keyline = NULL, keyfile = NULL, keydata = NULL)
}
\arguments{
  \item{obj}{An object containing the data imported by function \code{\link{read.formscanner}}.}
	\item{keyline}{the number of line of the data containing the correct responses to the items. 
		This line is deleted from the data and used as key.}
	\item{keyfile}{the name of the file with the correct responses to the items. 
		Column names should match the names of the items.}
	\item{keydata}{an R data frame containing the correct responses to the items. 
		Column names should match the names of the items.}
}
\details{
\code{keyfile} and \code{keydata} can contain more items than \code{obj}.
In this case, items not contained in \code{obj} are dropped in the output.
}
\value{
A list with \code{data}, \code{key}, and eventually \code{weights}.
}
\author{Michela Battauz}
\seealso{
\code{\link{addweights}}
}
\examples{
data(test)
data(key)
testk <- addkey(test, keydata = key)
}

