\name{report}
\alias{report}
\title{Report the Responses}
\description{
This function produces a graphic with the responses given by one or a few persons and shows the correct ones. 
}
\usage{
report(obj, columns, whichid, grid = TRUE, main = "", las = 0)
}
\arguments{
	\item{obj}{An object of class \code{fsdata} containing the data imported by function \code{\link{read.formscanner}}.
		\code{key} should not be \code{NULL}.}
	\item{columns}{A vector containing which columns to use. Columns can be specified by name or number.}
	\item{whichid}{A vector containing the values of variable \code{id} that are shown on the graph.}
	\item{grid}{logical; if \code{TRUE} horizontal lines are drown on the graph.}
	\item{main}{an overall title for the plot.}
	\item{las}{numeric in {0,1,2,3}; the style of axis labels (see \code{\link{par}}).}
}
\details{
Correct responses are colored green, wrong responses are colored red.
}
\author{Michela Battauz}
\seealso{
\code{\link{read.formscanner}}
}
\examples{
data(exam1)
report(obj = exam1, col = 2:31, whichid = c("101844", "101272"))
report(obj = exam1, col = c("I2", "I5", "I6", "I8", "I10", "I14", "I22"), 
	whichid = exam1$data$id[1:10], las= 3)
}