\name{mchoice2id}
\alias{mchoice2id}
\docType{data}
\title{Simulated Test Responses}
\description{
This data set contains simulated multiple choice responses to 30 items.
}
\usage{data("mchoice1id")}
\format{
A data frame with 1000 observations on the following 37 variables.
\tabular{rl}{
	\code{File.name} \tab file name. \cr
	\code{I21-I50} \tab responses given to the items. \cr
	\code{N1-N6} \tab concatenated, these form the identification number.}
}
\details{
	Columns \code{I21-I50} are the same of data set \code{\link{mchoice2}}.
	Column \code{File.name} of data set \code{\link{mchoice2}} can be obtained by concatenating columns \code{N1-N6}.
	This data set shares 10 common items with the \code{\link{mchoice1id}} data set.
	Column names \code{I21-I50} uniquely identify the items.
}
\author{Michela Battauz}
\seealso{
	\code{\link{key}}, \code{\link{read.formscanner}}
}
\examples{
data(mchoice2id)
head(mchoice2id)
}
\keyword{datasets}
