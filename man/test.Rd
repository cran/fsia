\name{test}
\alias{test}
\docType{data}
\title{Test Responses}
\description{
This data set contains multiple choice responses to 40 items.
}
\usage{data("test")}
\format{
  A data frame with 5 observations on the following 44 variables.
\tabular{ll}{
	\code{File.name} \tab file name. \cr
	\code{Question01-Question40} \tab responses given to the items. \cr
	\code{i.course} \tab course, \cr
  \code{i.university} \tab university. \cr
	\code{id} \tab identification number.}
}
\details{
This data set is obtained by importing file "scan_results_test.csv"
with function \code{\link{read.formscanner}}.
}
\author{Michela Battauz}
\seealso{
	\code{\link{key}}, \code{\link{read.formscanner}}, \code{\link{weights}}, \code{\link{weights_multiple}}
}
\examples{
data(test)
test
}
\keyword{datasets}
