\name{report}
\alias{report}
\title{Report the Responses}
\description{
This function produces a graphic with the responses given by one or a few subjects and shows the correct ones. 
}
\usage{
report(obj, columns, whichid, grid = TRUE, main = "", las = 0, itemlab = NULL, 
  weights = FALSE)
}
\arguments{
  \item{obj}{An object containing the data imported by function \code{\link{read.formscanner}}, 
		the key added by function \code{\link{addkey}} and/or weights added by function \code{\link{addweights}}.}
	\item{columns}{A vector containing which columns to use. Columns can be specified by name or number.}
	\item{whichid}{A vector containing the values of variable \code{id} that are shown on the graph.}
	\item{grid}{logical; if \code{TRUE} horizontal lines are drown on the graph.}
	\item{main}{an overall title for the plot.}
	\item{las}{numeric in {0,1,2,3}; the style of axis labels (see \code{\link{par}}).}
	\item{itemlab}{labels of the items.}
	\item{weights}{logical. If \code{TRUE} the weights are displayed.}
}
\details{
Correct responses are colored green, wrong responses are colored red.
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

par(mfrow = c(1, 2))
report(obj = testk, col = 2:11, whichid = c("102344", "245784"))
report(obj = testw, col = 2:11, whichid = c("102344", "245784"), weights = TRUE)
par(mfrow = c(1, 1))
report(obj = testwm, col = 2:11, whichid = c("102344", "245784"), weights = TRUE)
}
