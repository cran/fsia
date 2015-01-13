\name{mchoice1}
\alias{mchoice1}
\docType{data}
\title{Simulated Test Responses}
\description{
This data set contains simulated multiple choice responses to 30 items.
}
\usage{data("mchoice1")}
\format{
A data frame with 1000 observations on the following 31 variables.
\tabular{rl}{
	\code{File.name} \tab identification number. \cr
	\code{I1-I30} \tab responses given to the items. }
}
\details{
	This data set shares 10 common items with the \code{\link{mchoice2}} data set.
	Column names \code{I1-I30} uniquely identify the items.
}
\author{Michela Battauz}
\seealso{
	\code{\link{exam1}}, \code{\link{key}}, \code{\link{read.formscanner}}
}
\examples{
data(mchoice1)
head(mchoice1)
}
\keyword{datasets}
