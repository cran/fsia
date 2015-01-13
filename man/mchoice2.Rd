\name{mchoice2}
\alias{mchoice2}
\docType{data}
\title{Simulated Test Responses}
\description{
This data set contains simulated multiple choice responses to 30 items.
}
\usage{data("mchoice2")}
\format{
A data frame with 1000 observations on the following 31 variables.
\tabular{rl}{
	\code{File.name} \tab identification number. \cr
	\code{I21-I50} \tab responses given to the items. }
}
\details{
	This data set shares 10 common items with the \code{\link{mchoice1}} data set.
	Column names \code{I21-I50} uniquely identify the items.
}
\author{Michela Battauz}
\seealso{
	\code{\link{key}}, \code{\link{read.formscanner}}
}
\examples{
data(mchoice2)
head(mchoice2)
}
\keyword{datasets}
