\name{weights_multiple}
\alias{weights_multiple}
\docType{data}
\title{
Weights of each Response of the Items 
}
\description{
This data set contains the weights of each responses of the items in the test data set.
}
\usage{data("weights_multiple")}
\format{
  A data frame with variables
	\tabular{ll}{
		\code{response} \tab responses A, B, C, D. \cr
		\code{Question01-Question40} \tab weights for each response to each item.}
}
\author{Michela Battauz}
\seealso{
	\code{\link{addweights}}
}
\examples{
data(weights_multiple)
weights_multiple
}
\keyword{datasets}
